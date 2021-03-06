﻿namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequestByUser of UserId * Guid
    | CancelRequestByManager of UserId * Guid
    | RefuseRequest of UserId * Guid
    | CancelRequestRequest of UserId * Guid
    | CancelRequestRefuseRequest of UserId * Guid
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequestByUser (userId, _) -> userId
        | CancelRequestByManager (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId
        | CancelRequestRequest (userId, _) -> userId
        | CancelRequestRefuseRequest (userId, _) -> userId
// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCancelledByUser of TimeOffRequest
    | RequestCancelledByManager of TimeOffRequest
    | RequestRefuse of TimeOffRequest
    | RequestCancelRequest of TimeOffRequest
    | RequestCancelRequestRefuse of TimeOffRequest
    
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelledByUser request -> request
        | RequestCancelledByManager request -> request
        | RequestRefuse request -> request
        | RequestCancelRequest request -> request
        | RequestCancelRequestRefuse request -> request
        
// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | CancelledByUser of TimeOffRequest
        | CancelledByManager of TimeOffRequest
        | Refuse of TimeOffRequest
        | CancelRequest of TimeOffRequest
        | CancelRequestRefuse of TimeOffRequest
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request
            | Refuse request
            | CancelledByManager request
            | CancelRequest request
            | CancelRequestRefuse request
            | CancelledByUser request -> request
        member this.IsActive =
            match this with
            | CancelledByUser _
            | CancelledByManager _
            | CancelRequest _
            | CancelRequestRefuse _
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCancelledByUser request -> CancelledByUser request
        | RequestCancelledByManager request -> CancelledByManager request
        | RequestRefuse request -> Refuse request
        | RequestCancelRequest request -> CancelRequest request
        | RequestCancelRequestRefuse request -> CancelRequestRefuse request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)
    
    let overlapsWith request1 request2 =
       request1.UserId.Equals(request2.UserId) &&
       (((request1.Start.Date.CompareTo(request2.Start.Date) <= 0) && (request1.Start.Date.CompareTo(request2.End.Date) >= 0))
           || ((request1.End.Date.CompareTo(request2.Start.Date) <= 0) && (request1.End.Date.CompareTo(request2.End.Date) >= 0)))

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        otherRequests
        |> Seq.exists (overlapsWith request)             
          
    let createRequest activeUserRequests  request day =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= day then
            Error "The request starts in the past"
        else
            request.UpdateDate <- DateTime.Now
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
            
    let cancelRequestByUser requestState =
        match requestState with
        | PendingValidation request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestCancelledByUser request]
        | Validated request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestCancelledByUser request]
        | _ ->
            Error "Request cannot be cancel"
            
    let cancelRequestByManager requestState =
        match requestState with
        | PendingValidation request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestCancelledByManager request]
        | Validated request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestCancelledByManager request]
        | _ ->
            Error "Request cannot be cancel"
            
    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestRefuse request]
        | Refuse request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestRefuse request]
        | _ ->
            Error "Request cannot be refuse"
            
    let cancelRequest requestState =
        match requestState with
        | PendingValidation request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestCancelRequest request]
        | Refuse request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestCancelRequest request]
        | _ ->
            Error "Request cannot be cancel"           
            
    let cancelRequestRefuse requestState =
        match requestState with
        | PendingValidation request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestCancelRequestRefuse request]
        | Refuse request ->
            request.UpdateDate <- DateTime.Now
            Ok [RequestCancelRequestRefuse request]
        | _ ->
            Error "Request cannot be cancel"
            
    let getDayWithoutWeekend (dStart : DateTime) (dEnd : DateTime) =
        let mutable dayTotal = (float 0)
        let d = (dEnd - dStart).Days
        let mutable dateI = dStart 
        let mutable i = 0
        while i <= d  do
            if dateI.DayOfWeek.Equals DayOfWeek.Saturday || dateI.DayOfWeek.Equals DayOfWeek.Sunday then
                dayTotal <- dayTotal + 0.0
            else
                 dayTotal <- dayTotal + 1.0                 
            i <- i + 1
            dateI <- dateI.AddDays(1.0)
        dayTotal
        
    let getNumberDayAfterToday (userRequests : TimeOffRequest seq) (today: DateTime)=
        let mutable dayTotal = (float 0)
        let mutable dayWithoutWeekend = (float 0)
        for d in userRequests do
            if d.Start.Date.Year = today.Year &&  d.Start.Date.CompareTo(today) >= 0 then
                dayWithoutWeekend <- getDayWithoutWeekend d.Start.Date d.End.Date              
                dayTotal <- dayTotal + dayWithoutWeekend - 2.0
                if d.End.HalfDay = HalfDay.AM then
                    dayTotal <- dayTotal + 0.5
                else
                    dayTotal <- dayTotal + 1.0
                if d.Start.HalfDay = HalfDay.AM then
                    dayTotal <- dayTotal + 1.0
                else
                    dayTotal <- dayTotal + 0.5
            else
                dayTotal <- dayTotal
        dayTotal
        
    let getNumberDayBeforeToday (userRequests : TimeOffRequest seq) (today: DateTime)=
        let mutable dayTotal = (float 0)
        let mutable dayWithoutWeekend = (float 0)
        for d in userRequests do
            if d.Start.Date.Year = today.Year &&  d.Start.Date.CompareTo(today) <= 0 then
                dayWithoutWeekend <- getDayWithoutWeekend d.Start.Date d.End.Date
                dayTotal <- dayTotal + dayWithoutWeekend - 2.0
                if d.End.HalfDay = HalfDay.AM then
                    dayTotal <- dayTotal + 0.5
                else
                    dayTotal <- dayTotal + 1.0
                if d.Start.HalfDay = HalfDay.AM then
                    dayTotal <- dayTotal + 1.0
                else
                    dayTotal <- dayTotal + 0.5
            else
                dayTotal <- dayTotal
        dayTotal
    
    let getTimeOffPortion (date : DateTime) =
        ((float date.Month) - 1.0) * 2.5
    
    let getAllTimeOff (userId : UserId) (userRequests : TimeOffRequest seq) (date: DateTime)=   
        let portion = getTimeOffPortion date
        let carriedFromLastYear = 3.0   
        let takenToDate = getNumberDayBeforeToday userRequests date
        let planned = getNumberDayAfterToday userRequests date 
        let currentBalance = portion + carriedFromLastYear - (planned + takenToDate)
        Ok {UserId= userId; Portion= portion; CarriedFromLastYear = carriedFromLastYear; TakenToDate = takenToDate; Planned = planned; CurrentBalance = currentBalance}
    //TODO: Rename this method
    let decideBis (user: User) (userId: UserId) (userRequests: UserRequestsState) =
        match user with
        | Employee userId when userId <> userId ->
            Error "Unauthorized"
        | _ ->
            let activeUserRequests =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.where (fun state -> state.IsActive)
                |> Seq.map (fun state -> state.Request)
            getAllTimeOff userId activeUserRequests DateTime.Now
    let getAllRequests (user: User) (userId: UserId) (userRequests: List<TimeOffRequestHistory>) =
        match user with
        | Employee userId when userId <> userId ->
            Error "Unauthorized"
        | _ ->
            Ok userRequests

    let eventToString event =
        match event with
        | RequestCreated request -> "Pending validation"
        | RequestValidated request -> "Validated"
        | RequestCancelledByUser request -> "Cancelled by user"
        | RequestCancelledByManager request -> "Cancelled by manager"
        | RequestRefuse request -> "Refuse"
        | RequestCancelRequest request -> "Cancel Request"
        | RequestCancelRequestRefuse request -> "Cancel request refuse"
    
    let mapHistory (history: List<TimeOffRequestHistory>) (event: RequestEvent) =
        let eventString = eventToString event
        history @ [{
            Date= event.Request.UpdateDate;
            From= event.Request.Start;
            To= event.Request.End;
            Days= getDayWithoutWeekend event.Request.Start.Date event.Request.End.Date;
            Event= eventString
        }]
        
        
    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->                
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request DateTime.Today

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState
            | CancelRequestByUser (_, requestId) ->
                if user <> Employee(relatedUserId) then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelRequestByUser requestState
            | CancelRequestByManager (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelRequestByManager requestState
            | RefuseRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    refuseRequest requestState
            | CancelRequestRequest (_, requestId) ->
                if user <> Employee(relatedUserId) then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelRequest requestState
            | CancelRequestRefuseRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    cancelRequestRefuse requestState
            

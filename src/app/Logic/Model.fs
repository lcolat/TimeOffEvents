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
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
            
    let cancelRequestByUser requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelledByUser request]
        | Validated request ->
            Ok [RequestCancelledByUser request]
        | _ ->
            Error "Request cannot be cancel"
            
    let cancelRequestByManager requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelledByManager request]
        | Validated request ->
            Ok [RequestCancelledByManager request]
        | _ ->
            Error "Request cannot be cancel"
            
    let refuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefuse request]
        | Refuse request ->
            Ok [RequestRefuse request]
        | _ ->
            Error "Request cannot be refuse"
            
    let cancelRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelRequest request]
        | Refuse request ->
            Ok [RequestCancelRequest request]
        | _ ->
            Error "Request cannot be cancel"
            
    let cancelRequestRefuse requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestCancelRequestRefuse request]
        | Refuse request ->
            Ok [RequestCancelRequestRefuse request]
        | _ ->
            Error "Request cannot be cancel"
            
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
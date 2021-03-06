module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
    Expect.equal result expected message

open System

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let now = DateTime.Now
      let request = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let now = DateTime.Now
      let request1 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
  ]

let now = DateTime.Now

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = now.AddDays(1.0); HalfDay = AM }
        End = { Date = now.AddDays(1.0); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]
  
[<Tests>]  
let cancelByUserTest =
    testList "Cancel test by user" [
    test "A request is cancel" {
      let request = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (CancelRequestByUser ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByUser request]) "The request should have been cancelled"
    }
  ]
    
[<Tests>]  
let cancelByManagerTest =
    testList "Cancel test by manager" [
    test "A request is cancel" {
      let request = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequestByManager ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelledByManager request]) "The request should have been cancelled"
    }
  ]
    
[<Tests>]  
let RefuseTest =
    testList "Refuse test by manager" [
    test "A request is refuse" {
      let request = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (RefuseRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestRefuse request]) "The request should have been refuse"
    }
  ]
    
[<Tests>]  
let RequestCancelRequest =
    testList "Request cancel test by manager" [
    test "A request is request cancel" {
      let request = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> When (CancelRequestRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelRequest request]) "The request should have been request cancel"
    }
  ]
    
[<Tests>]  
let RequestCancelRequestRefuse =
    testList "Request cancel refuse test by manager" [
    test "A request is request cancel and refuse" {
      let request = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (CancelRequestRefuseRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelRequestRefuse request]) "The request should have been request cancel refuse"
    }
  ]
    
[<Tests>]  
let RequestGetWeekendDay =
    testList "Request get weekend day" [
    test "GetWeekendDay give weekend day" {
      let DateS = DateTime(2020, 01, 02)
      let DateE = DateTime(2020, 01, 08)
      Expect.equal (Logic.getDayWithoutWeekend DateS DateE) 5.0 "GetWeekenDay send weekend day"
    }
  ]
    
[<Tests>]  
let RequestGetPortion =
    testList "Request get Portion" [
    test "Portion give portion" {
      let DateS = DateTime(2020, 04, 02)
      Expect.equal (Logic.getTimeOffPortion DateS) 7.5 "Portion is ok"
    }
  ]
    
[<Tests>]  
let RequestGetTakeToDate =
    testList "Request get Take to date" [
    test "Requests on 2 distinct days for take to date" {
      let now = DateTime.Now
      let request1 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 03); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 03); HalfDay = PM }
      }

      let request2 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 08); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 08); HalfDay = PM }
      }
      let request3 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 06, 05); HalfDay = AM }
        End = { Date = DateTime(2020, 06, 05); HalfDay = PM }
      }
      let array = [|request1;request2;request3|]
      let sequence = array :> seq<TimeOffRequest>
      Expect.equal (Logic.getNumberDayBeforeToday sequence (DateTime(2020, 01, 10))) 2.0 "TakenToDate is ok"
    }
  ]
    
[<Tests>]  
let RequestGetPlanned =
    testList "Request get Planned" [
    test "Requests on 2 distinct days for planned" {
      let request1 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 03); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 03); HalfDay = PM }
      }

      let request2 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 17); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 17); HalfDay = PM }
      }
      let request3 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 06, 05); HalfDay = AM }
        End = { Date = DateTime(2020, 06, 05); HalfDay = PM }
      }
      let array = [|request1;request2;request3|]
      let sequence = array :> seq<TimeOffRequest>
      Expect.equal (Logic.getNumberDayAfterToday sequence (DateTime(2020, 01, 10))) 2.0 "planned is ok"
    }
  ]
    
[<Tests>]  
let RequestGetAllTimeOff =
    testList "Request get All timeoff" [
    test "Requests on 2 distinct days for all time off" {
      let request1 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 03); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 03); HalfDay = PM }
      }

      let request2 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 01, 17); HalfDay = AM }
        End = { Date = DateTime(2020, 01, 17); HalfDay = PM }
      }
      let request3 = {
        UpdateDate = now
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2020, 06, 05); HalfDay = AM }
        End = { Date = DateTime(2020, 06, 05); HalfDay = PM }
      }   
      let array = [|request1;request2;request3|]
      let sequence = array :> seq<TimeOffRequest>
      Expect.equal (Logic.getAllTimeOff request1.UserId sequence (DateTime(2020, 01, 10))) (Ok {UserId= "jdoe"; Portion= 0.0; CarriedFromLastYear = 3.0; TakenToDate = 1.0; Planned = 2.0; CurrentBalance = 0.0}) "all timeoff is ok"
    }
  ] 
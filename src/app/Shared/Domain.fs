namespace TimeOff

open System

// First, we define our domain
type UserId = string

type User =
    | Employee of UserId
    | Manager

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

[<CLIMutable>]
type TimeOffRequestHistory = {
    Date: DateTime
    From: Boundary
    To: Boundary
    Days: float
    Event: string
}

[<CLIMutable>]
type TimeOffDay = {
    UserId: UserId
    Portion: float
    CarriedFromLastYear : float
    TakenToDate : float
    Planned : float
    CurrentBalance : float
}

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
    mutable UpdateDate: DateTime
}



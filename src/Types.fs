module Types

type Deferred<'T> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 'T

type Item =
  { albumId: int
    id: int
    title: string
    url: string
    thumbnailUrl: string }

type AsyncOperationStatus<'T> =
    | Started
    | Finished of 'T
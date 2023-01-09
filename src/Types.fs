module Types

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type Item = {
    albumId: int
    id: int
    title: string
    url: string
    thumbnailUrl: string
  }

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't
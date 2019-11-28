# tunebank

WORK IN PROGRESS

This experimental project is intended eventually to be a complete replacement for the [musicrest](https://github.com/newlandsvalley/musicrest) project which is a RESTFUL service for serving up traditional tunes in ABC format.  Musicrest was written in Scala 2.11.7 with the database provided by MongoDB 2.4 via the Casbah API.  It has been in production for 6 years at the time of writing but I now consider it to be too costly to maintain - largely because the Mongo version and Casbah itself have reached end-of-life status and the upgrade path is tortuous.

The initial experiment is to investigate Haskell's Servant in order to support the existing RESTful API which itself will be pared down to just those endpoints that are actually used in [tunebank-frontend](https://github.com/newlandsvalley/tunebank-frontend).  The tunes that are served up at this stage will simply consist of test data held in memory.

I will then probably investigate Postgres as the database provider.  At this point we will have implemented the replacement using the so-called PHP stack.  

The final stage will be to provide improvements to the API which are mostly already identified in the musicrest issues list.

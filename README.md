# tunebank

WORK IN PROGRESS

This experimental project is intended eventually to be a complete replacement for the [musicrest](https://github.com/newlandsvalley/musicrest) project which is a RESTful service for serving up traditional tunes in ABC format.  Musicrest was written in Scala 2.11.7 with the database provided by MongoDB 2.4 via the Casbah API.  It has been in production for 6 years at the time of writing but I now consider it to be too costly to maintain - largely because the Mongo version and Casbah itself have reached end-of-life status and the upgrade path is tortuous.

The initial experiment is to investigate Haskell's Servant in order to support the existing RESTful API which itself will be pared down to just those endpoints that are actually used in [tunebank-frontend](https://github.com/newlandsvalley/tunebank-frontend).  The tunes that are served up at this stage will simply consist of test data held in memory.

I will then probably investigate Postgres as the database provider.  At this point we will have implemented the replacement using the so-called PHP stack.  

The final stage will be to provide improvements to the API which are mostly already identified in the musicrest issues list.

Transcoding Utilities
---------------------

I am currently using (under Ubuntu 18.04):

* abcm2ps-8.14.4 (2019-03-18)
* abc2midi 4.22 June 20 2019 abc2midi
* convert: ImageMagick 6.9.7-4 Q16 x86_64 20170114

Note that now Ubuntu has rather draconian security policies which by default disable most of the transcoding I need to perform.  The policy file is /etc/ImageMagick-6/policy.xml which I have edited in the following way in order to allow read/write of the file types I need:

```
  <policy domain="coder" rights="read|write" pattern="PS" />
  <policy domain="coder" rights="none" pattern="PS2" />
  <policy domain="coder" rights="none" pattern="PS3" />
  <policy domain="coder" rights="read|write" pattern="EPS" />
  <policy domain="coder" rights="read|write" pattern="PDF" />
  <policy domain="coder" rights="read|write" pattern="XPS" />

```

# tunebank

WORK IN PROGRESS

This experimental project is intended eventually to be a complete replacement for the [musicrest](https://github.com/newlandsvalley/musicrest) project which is a RESTful service for serving up traditional tunes in ABC format.  Musicrest was written in Scala 2.11.7 with the database provided by MongoDB 2.4 via the Casbah API.  It has been in production for 6 years at the time of writing but I now consider it to be too costly to maintain - largely because the Mongo version and Casbah itself have reached end-of-life status and the upgrade path is tortuous.

It uses Haskell's [Servant](https://www.servant.dev/) in order to support the existing RESTful API which itself is be pared down to just those endpoints that are actually used in [tunebank-frontend](https://github.com/newlandsvalley/tunebank-frontend) (the haskell-backend branch).  The database is Postgresql accessed by means of [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple). 

Database calls in the test suite are mocked using a typeclass-based approach culled in the most part from [here](https://functor.tokyo/blog/2015-11-20-testing-db-access). I find this approach effective, but it comes at a considerable cost to the readability of the code.

A final stage will be to provide improvements to the API which are mostly already identified in the musicrest issues list.

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

Problems
--------

I have one problem to overcome at the moment - how to validate new user registration. Currently, it uses musicrest's approach of sending an email to the user with a link to a slug that the user must click on to complete the registration process.  I use a gmail account to send these messages.

Since musicrest was developed, gmail's security has tightened considerably.  It now refuses to allow the validation message to be sent because it comes from an untrusted device.  It suggests moving to [2-step verification](https://www.google.com/landing/2step/).  I am not at all clear how this should be done in my environment.

Any help would be gratefully accepted.
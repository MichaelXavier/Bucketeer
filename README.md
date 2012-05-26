Bucketeer
---------

Bucketeer is a lightweight web service used for governing other rate limited
services via a leaky bucket algorithm. Bucketeer is written in Haskell and uses
Redis for persistence. 

Project Status
==============
I am currently putting the finishing touches on this project. Suggestions and
pull requests are welcome. The minimum viable HTTP interface is tested. Some
tasks left to do:

1. Authentication?
2. Release to Hackage


Basics
======
Bucketeer has the concepts of **consumer** and **feature**. A **consumer** is a
uniquely-identifying name for a user of your api service that may be rate
limited. A consumer is entitled to many features. A **feature** is some uniqely
identifying name of a feature in your service that has a particular limit and
restore rate.

Say you have an API. Joe is a consumer, making a request to list products in a
catalog. You might structure your API with middleware to authenticate the user.

```
+---------------+    +--------------+    +----------------+             +-----------------+
| Joe's Request |===>| Authenticate |===>| Bucketeer Tick |== 200 OK ==>| Perform Request |
+---------------+    +--------------+    +----------------+             +-----------------+
                                                ||
                                                ||
                                       420 Enhance Your Calm
                                                ||
                                                ||
                                                \/
+---------------+                        +----------------+                                
| Throttle Resp |<=======================|  Render Error  |                                
+---------------+                        +----------------+                                
      
```

If the tick request is successful, it will return a 200 with the remaining
account in JSON. The user shuld be able to proceed with their action. If a 420
response comes back, the user is throttled and should receive the appropriate
status from you, such as 503.


Setup
=====
To build Bucketeer you'll need cabal-dev:

```
cabal install cabal-dev
```

To build and run the test suite with cabal, run 
```
make spec
```
If you have all the dependencies installed, it is much easier to just run

```
make quick_spec
```

To build the project, simply run

```
make
```
The executable will be `dist/build/bucketeer/bucketeer`


Running Bucketeer
=================
```
./bucketeer --help
Help Options:
  -h, --help                  Show option summary.
  --help-all                  Show all help options.

Application Options:
  -p, --port                  Port. Default 3000
  -n, --namespcae             Optional redis namespace so multiple bucketeers can run on the same Redis instance
  --redis-host                Redis host. Default localhost
  --redis-port                Redis port. Default 6379
  --redis-password            Redis password. Default no password.
  --redis-max-connections     Redis max connections. Default 50.
  --redis-max-idle            Redis max idle time in seconds. Default 30.
  -l, --log-file              Log file. Defaults to only logging to stdout.

```

Bucketeer currently has no authentication system. This should be perfectly fine
as long as you don't configure it to be internet-facing.

Bucketeer will persist all data to redis, so if you want your bucket data to be
durable, be sure to configure redis appropriately to write to disk. When
Bucketeer exits or its list of buckets is modified, it will dump its
configuration to redis as JSON under the key "bucketeer:manager". Upon starting
Bucketeer, it will reload this config. If the config somehow becomes hosed, you
can always delete that key and Bucketeer will start fresh.

If you need to run multiple, logically separate instances of Bucketeer, pass
the -n flag to the executable to specify a namespace. This way, Bucketeer will
persist and read from separate locations for each instance.

Deployment
==========
Bucketeer supports deployment through [keter](https://github.com/snoyberg/keter).

Make sure to copy `config/keter.yaml.sample` to `config/keter.yaml` and make
any changes you need for deployment.

running `make` will generate a `bucketeer.keter` file. Sync this to your keter
incoming directory for zero downtime deployment. Note that Bucketeer will take
its port from the `PORT` environment variable if its set, as keter expects, so
you shouldn't set that in your keter.yaml file as a flag, because keter
determines this at runtime.

Commands
========

Bucketeer is managed entirely through HTTP. 

Getting List of Buckets
=======================
Send a GET to /

```
curl -X GET http://localhost:3000/

[{"restore_rate":90,"capacity":10,"feature":"check_messages","consumer":"michael"}]
```

Getting Remaining Request Count 
===============================
Send a GET to /consumers/consumername/buckets/featurename

```
curl -X GET http://localhost:3000/consumers/michael/buckets/check_messages

{"remaining":9}
```

Creating/Replacing a Bucket
===========================
Send a POST to /consumers/consumername/buckets/featurename
Requires the following parameters:

**capacity**: number of requests their bucket can hold 

**restore_rate**: how many *milliseconds* to wait before restoring 1 request to that particular bucket.


```
curl -X POST -d "capacity=10&restore_rate=9000" http://localhost:3000/consumers/michael/buckets/check_messages
```

This bucket will restore 1 request every 9 seconds.

The URL for the bucket will be returned in the *Location* header.

Ticking a Bucket
================
Ticking expends one request in the user's bucket if they have at least one remaining.

Send a POST to /consumers/consumername/buckets/featurename/tick

If the user has enough remaining requests, the response will be a 200 and will
return the remaining count:

```
curl -X POST http://localhost:3000/consumers/michael/buckets/check_messages/tick

{"remaining":9}
```

However, if the user does not have enough remaining requests, you will receive
a 420 Enhance Your Calm status code and an error message:

```
curl -X POST http://localhost:3000/consumers/michael/buckets/check_messages/tick

{"description":"check_messages bucket has been exhausted for michael","id":"Bucket Exhausted"}
```

Refilling a Bucket
==================
Refilling a bucket resets the remaining requests count to the full capacity.

Send a POST to /consumers/consumername/buckets/featurename/refill

```
curl -X POST http://localhost:3000/consumers/michael/buckets/check_messages/refill

{"remaining":10}
```

Draining a Bucket
=================
Draining a bucket sets the remaining requests count to 0. Note that the bucket
will still continue to be refilled at the normal rate.

Send a POST to /consumers/consumername/buckets/featurename/drain

```
curl -X POST http://localhost:3000/consumers/michael/buckets/check_messages/drain

```

Deleting a Bucket
=================
Deleting a bucket will remove it from Bucketeer and stop it from refilling.
Subsequent requests to this bucket will return 404s.

Send a DELETE to /consumers/consumername/buckets/featurename

```
curl -X DELETE http://localhost:3000/consumers/michael/buckets/check_messages/drain

```

Deleting a Consumer
===================
Deleting a consumer will delete and halt all of their buckets.

Send a DELETE to /consumers/consumername

```
curl -X DELETE http://localhost:3000/consumers/michael

```

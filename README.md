Bucketeer
---------

Bucketeer is a lightweight web service used for governing other rate limited
services via a leaky bucket algorithm. Bucketeer is written in Haskell and uses
Redis for persistence. 

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
| Throttle Resp |<=======================| Bucketeer Tick |                                
+---------------+                        +----------------+                                
      
```

If the tick request is successful, it will return a 200 with the remaining
account in JSON. The user shuld be able to proceed with their action. If a 420
response comes back, the user is throttled and should receive the appropriate
status from you, such as 503.

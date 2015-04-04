:warning:
**This is work in progress. Use [LDAP][LDAP] if you need LDAP integration.**

ldap-client
===========
[![Build Status](https://travis-ci.org/supki/ldap-client.svg?branch=master)](https://travis-ci.org/supki/ldap-client)

This library implements (the parts of) [RFC 4511][rfc4511]

          Feature            | RFC Section |   Support
:--------------------------- |:-----------:|:-----------:
Bind Operation               | 4.2         | ✔
Unbind Operation             | 4.3         | ✔
Notice of Disconnection      | 4.4.1       | ✘
Search Operation             | 4.5         | ✔\*
Modify Operation             | 4.6         | ✔
Add Operation                | 4.7         | ✔
Delete Operation             | 4.8         | ✔
Modify DN Operation          | 4.9         | ✔
Compare Operation            | 4.10        | ✔
Abandon Operation            | 4.11        | ✘
Extended Operation           | 4.12        | ✔
IntermediateResponse Message | 4.13        | ✘
StartTLS Operation           | 4.14        | ✔†
LDAP over TLS                | -           | ✔

\* The `:dn` thing is unsupported in Extensible matches  
† Only serves as an example of Extended Operation.  It's useless for all practical purposes as it does not actually enable TLS.  In other words, use LDAP over TLS instead.

  [rfc4511]: https://tools.ietf.org/html/rfc4511
  [LDAP]: https://hackage.haskell.org/package/LDAP

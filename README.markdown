ldap-client
===========
[![Build Status](https://travis-ci.org/supki/ldap-client.svg?branch=master)](https://travis-ci.org/supki/ldap-client)

This library implements (the parts of) [RFC 4511][rfc4511]

          Feature            |   RFC Section   |   Support
:--------------------------- |:---------------:|:-----------:
Bind Operation               | [4.2][4.2]      | ✔
Unbind Operation             | [4.3][4.3]      | ✔
Unsolicited Notification     | [4.4][4.4]      | ✔
Notice of Disconnection      | [4.4.1][4.4.1]  | ✔
Search Operation             | [4.5][4.5]      | ✔\*
Modify Operation             | [4.6][4.6]      | ✔
Add Operation                | [4.7][4.7]      | ✔
Delete Operation             | [4.8][4.8]      | ✔
Modify DN Operation          | [4.9][4.9]      | ✔
Compare Operation            | [4.10][4.10]    | ✔
Abandon Operation            | [4.11][4.11]    | ✘
Extended Operation           | [4.12][4.12]    | ✔
IntermediateResponse Message | [4.13][4.13]    | ✔
StartTLS Operation           | [4.14][4.14]    | ✔†
LDAP over TLS                | -               | ✔

\* The `:dn` thing is unsupported in Extensible matches  
† Only serves as an example of Extended Operation.  It's useless for all practical purposes as it does not actually enable TLS.  In other words, use LDAP over TLS instead.

  [rfc4511]: https://tools.ietf.org/html/rfc4511
  [LDAP]: https://hackage.haskell.org/package/LDAP
  [4.2]: https://tools.ietf.org/html/rfc4511#section-4.2
  [4.3]: https://tools.ietf.org/html/rfc4511#section-4.3
  [4.4]: https://tools.ietf.org/html/rfc4511#section-4.4
  [4.4.1]: https://tools.ietf.org/html/rfc4511#section-4.4.1
  [4.5]: https://tools.ietf.org/html/rfc4511#section-4.5
  [4.6]: https://tools.ietf.org/html/rfc4511#section-4.6
  [4.7]: https://tools.ietf.org/html/rfc4511#section-4.7
  [4.8]: https://tools.ietf.org/html/rfc4511#section-4.8
  [4.9]: https://tools.ietf.org/html/rfc4511#section-4.9
  [4.10]: https://tools.ietf.org/html/rfc4511#section-4.10
  [4.11]: https://tools.ietf.org/html/rfc4511#section-4.11
  [4.12]: https://tools.ietf.org/html/rfc4511#section-4.12
  [4.13]: https://tools.ietf.org/html/rfc4511#section-4.13
  [4.14]: https://tools.ietf.org/html/rfc4511#section-4.14

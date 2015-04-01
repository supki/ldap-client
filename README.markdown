**NOTE: This is work in progress. Don't use it! If you really need LDAP integration, check out [LDAP][LDAP]**

ldap-client
===========

This library implements (the parts of) [RFC 4511][rfc4511]

          Feature            | RFC Section |   Support
:--------------------------- |:-----------:|:-----------:
Bind Operation               | 4.2         | ✔
Unbind Operation             | 4.3         | ✔
Notice of Disconnection      | 4.4.1       | ✘
Search Operation             | 4.5         | ✔ (partial)
Modify Operation             | 4.6         | ✘
Add Operation                | 4.7         | ✔
Delete Operation             | 4.8         | ✔
Modify DN Operation          | 4.9         | ✘
Compare Operation            | 4.10        | ✔
Abandon Operation            | 4.11        | ✘
Extended Operation           | 4.12        | ✘
IntermediateResponse Message | 4.13        | ✘
StartTLS Operation           | 4.14        | ✘
LDAP over TLS                | -           | ✔

```
% git grep '\bString\b' | wc -l
2
```

Testing
-------

```shell
% sudo apt-get install npm
% npm install ldapjs
% cabal test
```

  [rfc4511]: https://tools.ietf.org/html/rfc4511
  [LDAP]: https://hackage.haskell.org/package/LDAP

Original Changelog
====
  * See `CHANGELOG.md` for recent changes

0.2.0
=====

  * Supported SASL authentication via the EXTERNAL mechanism. (https://github.com/supki/ldap-client/pull/9)

  * Added the `SecureWithTLSSettings` constructor to the `Host` datatype for the
    cases where the user needs more control over TLS connection settings.
    (https://github.com/supki/ldap-client/issues/5, https://github.com/supki/ldap-client/pull/6)

  * Switched the decoding of server's messages to BER (See https://tools.ietf.org/html/rfc4511#section-5.1) (https://github.com/supki/ldap-client/pull/11)

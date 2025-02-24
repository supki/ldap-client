0.4.0
===

  * I don't think a major version bump was strictly necessary, but did it anyway to be on the safe side.

  * Supported lts-22.43 (GHC 9.6.6)

0.3.0
===

  * Supported lts-19.33 (GHC 9.0.2)

  * The package got stolen by an already dead company (KEKW), so I had to rename it

0.2.0
===

  * Supported SASL authentication via the EXTERNAL mechanism. (https://github.com/supki/ldap-client/pull/9)

  * Added the `SecureWithTLSSettings` constructor to the `Host` datatype for the
    cases where the user needs more control over TLS connection settings.
    (https://github.com/supki/ldap-client/issues/5, https://github.com/supki/ldap-client/pull/6)

  * Switched the decoding of server's messages to BER (See https://tools.ietf.org/html/rfc4511#section-5.1) (https://github.com/supki/ldap-client/pull/11)

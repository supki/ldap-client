{ mkDerivation, asn1-encoding, asn1-types, async, base, bytestring
, connection, containers, doctest, hspec, network, process
, semigroups, stdenv, stm, text
}:
mkDerivation {
  pname = "ldap-client";
  version = "0.4.0";
  src = ./.;
  buildDepends = [
    asn1-encoding asn1-types async base bytestring connection
    containers network semigroups stm text
  ];
  testDepends = [ base bytestring doctest hspec process semigroups ];
  homepage = "https://supki.github.io/ldap-client";
  description = "Pure Haskell LDAP Client Library";
  license = stdenv.lib.licenses.bsd2;
}

#!/usr/bin/env nodejs

var fs = require('fs');
var ldapjs = require('ldapjs');

var port = process.env.PORT;
var certificate = fs.readFileSync(process.env.SSL_CERT, "utf-8");
var key = fs.readFileSync(process.env.SSL_KEY, "utf-8");
var server = ldapjs.createServer({certificate: certificate, key: key});

// <http://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_National_Pok%C3%A9dex_number>
var pokemon = [
  { dn: 'cn=bulbasaur,o=localhost',
    attributes: { cn: 'bulbasaur', evolution: "0", type: ["grass", "poison"], }
  },
  { dn: 'cn=ivysaur,o=localhost',
    attributes: { cn: 'ivysaur', evolution: "1", type: ["grass", "poison"], }
  },
  { dn: 'cn=venusaur,o=localhost',
    attributes: { cn: 'venusaur', evolution: "2", type: ["grass", "poison"], }
  },
  { dn: 'cn=charmander,o=localhost',
    attributes: { cn: 'charmander', evolution: "0", type: ["fire"], }
  },
  { dn: 'cn=charmeleon,o=localhost',
    attributes: { cn: 'charmeleon', evolution: "1", type: ["fire"], }
  },
  { dn: 'cn=charizard,o=localhost',
    attributes: { cn: 'charizard', evolution: "2", type: ["fire", "flying"], }
  },
  { dn: 'cn=squirtle,o=localhost',
    attributes: { cn: 'squirtle', evolution: "0", type: ["water"], }
  },
  { dn: 'cn=wartortle,o=localhost',
    attributes: { cn: 'wartortle', evolution: "1", type: ["water"], }
  },
  { dn: 'cn=blastoise,o=localhost',
    attributes: { cn: 'blastoise', evolution: "2", type: ["water"], }
  },
  { dn: 'cn=caterpie,o=localhost',
    attributes: { cn: 'caterpie', evolution: "0", type: ["bug"], }
  },
  { dn: 'cn=metapod,o=localhost',
    attributes: { cn: 'metapod', evolution: "1", type: ["bug"], }
  },
  { dn: 'cn=butterfree,o=localhost',
    attributes: { cn: 'butterfree', evolution: "2", type: ["bug", "flying"], }
  },
  { dn: 'cn=pikachu,o=localhost',
    attributes: { cn: 'pikachu', evolution: "0", type: ["electric"], password: "i-choose-you" }
  },
  ];

server.bind('cn=admin', function(req, res, next) {
  if ((req.dn.toString() === 'cn=admin') && (req.credentials === 'secret')) {
    res.end();
    return next();
  } else {
    return next(new ldapjs.InvalidCredentialsError());
  }
});

server.bind('cn=pikachu,o=localhost', function(req, res, next) {
  if ((req.dn.toString() === 'cn=pikachu,o=localhost') && (req.credentials === 'i-choose-you')) {
    res.end();
    return next();
  } else {
    return next(new ldapjs.InvalidCredentialsError());
  }
});

function authorize(req, res, next) {
  var bindDN = req.connection.ldap.bindDN;
  if ((bindDN.equals('cn=admin')) ||
      (bindDN.equals('cn=anonymous'))) {
    return next();
  } else {
    return next(new ldapjs.InsufficientAccessRightsError());
  }
}

server.search('o=localhost', [authorize], function(req, res, next) {
  for (var i = 0; i < pokemon.length; i++) {
    if (req.filter.matches(pokemon[i].attributes))
      res.send(pokemon[i]);
  };

  res.end();
  return next();
});

server.add('o=localhost', [], function(req, res, next) {
  var attributes = req.toObject().attributes;
  pokemon.push(req.toObject())
  res.end();
  return next();
});

server.del('o=localhost', [], function(req, res, next) {
  for (var i = 0; i < pokemon.length; i++) {
    if (req.dn.toString() == pokemon[i].dn) {
      pokemon.splice(i, 1);
      res.end();
      return next();
    }
  }

  return next(new ldapjs.NoSuchObjectError(req.dn.toString()));
});

server.listen(port, function() {
  console.log("ldaps://localhost:%d", port);
});

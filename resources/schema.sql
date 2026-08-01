drop table if exists rsvp;
drop table if exists gift;
drop table if exists details;

create table details (
    details TEXT NOT NULL,
    date    DATETIME NOT NULL
);

create table gift (
    id          TEXT NOT NULL,
    purchasedAt DATETIME NOT NULL,
    purchasedBy TEXT NOT NULL,
    amount      REAL NULL,
    notes       TEXT NOT NULL
);

create table rsvp (
    name     TEXT PRIMARY KEY,
    people   TEXT NOT NULL,
    children INTEGER NOT NULL,
    infants  INTEGER NOT NULL,
    updated  DATETIME NOT NULL,
    regards  TEXT NOT NULL
);

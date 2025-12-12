drop table if exists rsvp;
drop table if exists gift;

create table gift (
    id             TEXT NOT NULL,
    purchasedAt    DATETIME NOT NULL,
    purchasedBy    TEXT NOT NULL,
    amount         REAL NULL
);

create table rsvp (
    name      TEXT PRIMARY KEY,
    people    TEXT NOT NULL,
    children  INTEGER NOT NULL,
    infants   INTEGER NOT NULL,
    updated   DATETIME NOT NULL
);

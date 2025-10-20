drop table link;
drop table rsvp;
drop table registry;

create table registry (
    item           TEXT,
    purchasedAt    INTEGER,
    purchasedBy    TEXT
);

create table rsvp (
    name      TEXT PRIMARY KEY,
    attending BOOLEAN,
    infants   INTEGER,
    children  INTEGER,
    updated   DATETIME
);

create table link (
    original TEXT REFERENCES rsvp(name),
    linked   TEXT REFERENCES rsvp(name)
);

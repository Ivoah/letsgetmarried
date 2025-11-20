drop table if exists rsvpLink;
drop table if exists rsvp;
drop table if exists registryPurchase;

create table registryPurchase (
    id             TEXT,
    amount         INTEGER,
    purchasedAt    DATETIME,
    purchasedBy    TEXT
);

create table rsvp (
    name      TEXT PRIMARY KEY,
    attending BOOLEAN,
    infants   INTEGER,
    children  INTEGER,
    updated   DATETIME
);

create table rsvpLink (
    original TEXT REFERENCES rsvp(name),
    linked   TEXT REFERENCES rsvp(name)
);

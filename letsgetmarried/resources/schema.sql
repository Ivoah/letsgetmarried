drop table rsvpLink;
drop table rsvp;
drop table registryPurchase;

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

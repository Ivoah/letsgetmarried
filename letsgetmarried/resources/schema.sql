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
    adults    INTEGER,
    children  INTEGER,
    infants   INTEGER,
    updated   DATETIME
);

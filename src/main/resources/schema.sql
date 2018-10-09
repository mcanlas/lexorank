CREATE TABLE rankable_entities (
    id      serial       primary key,
    payload varchar(255) NOT NULL,
    rank    INT          NOT NULL
);
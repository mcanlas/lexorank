CREATE TABLE rankable_entities (
    id      serial       primary key,
    payload varchar(255) NOT NULL,
    rank    INT          NOT NULL
);

insert into rankable_entities values (1, 'asdf', 3);
insert into rankable_entities values (2, 'asdf', 7);
drop table if exists test_groups;
create table test_groups (
        id integer primary key autoincrement not null,
        description varchar(255) not null
);

drop table if exists test_group_memberships;
create table test_group_memberships (
        id integer primary key autoincrement not null,
        group_id int not null,
        test_id varchar(255)
);

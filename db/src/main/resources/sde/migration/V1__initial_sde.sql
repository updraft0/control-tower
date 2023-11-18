-- ATTACH DATABASE '' AS sde;

-- dogma_attribute_category
CREATE TABLE sde.dogma_attribute_category
(
    id          INTEGER PRIMARY KEY,
    name        TEXT NOT NULL,
    description TEXT
) STRICT;

-- dogma_attribute_type
CREATE TABLE sde.dogma_attribute_type
(
    id            INTEGER PRIMARY KEY,
    category_id   INTEGER,
    data_type     INTEGER NOT NULL,
    name          TEXT    NOT NULL,
    description   TEXT,
    default_value REAL    NOT NULL,
    unit_id       INTEGER,
    icon_id       INTEGER,

    FOREIGN KEY (category_id) REFERENCES dogma_attribute_category (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- item_category
CREATE TABLE sde.item_category
(
    id      INTEGER PRIMARY KEY,
    name    TEXT NOT NULL UNIQUE,
    icon_id INTEGER
) STRICT;

-- item_group
CREATE TABLE sde.item_group
(
    id          INTEGER PRIMARY KEY,
    category_id INTEGER NOT NULL,
    name        TEXT    NOT NULL,
    icon_id     INTEGER,

    FOREIGN KEY (category_id) REFERENCES item_category (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- item_name
CREATE TABLE sde.item_name
(
    id       INTEGER PRIMARY KEY,
    group_id INTEGER NOT NULL,
    name     TEXT    NOT NULL,

    FOREIGN KEY (group_id) REFERENCES item_group (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- item_type
CREATE TABLE sde.item_type
(
    id          INTEGER PRIMARY KEY,
    name        TEXT    NOT NULL,
    group_id    INTEGER NOT NULL,
    description TEXT,
    mass        REAL,
    volume      REAL,

    FOREIGN KEY (group_id) REFERENCES item_group (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- item_dogma_attribute
CREATE TABLE sde.item_dogma_attribute
(
    item_id      INTEGER NOT NULL,
    attribute_id INTEGER NOT NULL,
    value        REAL    NOT NULL,

    FOREIGN KEY (item_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (attribute_id) REFERENCES dogma_attribute_type (id) DEFERRABLE INITIALLY DEFERRED,
    UNIQUE (item_id, attribute_id)
) STRICT;

-- station_service
CREATE TABLE sde.station_service
(
    id   INTEGER PRIMARY KEY,
    name TEXT NOT NULL
) STRICT;

-- station_operation
CREATE TABLE sde.station_operation
(
    id          INTEGER PRIMARY KEY,
    activity_id INTEGER NOT NULL,
    name        TEXT    NOT NULL,
    description TEXT
) STRICT;

-- station_operation_service
CREATE TABLE sde.station_operation_service
(
    operation_id INTEGER NOT NULL,
    service_id   INTEGER NOT NULL,

    FOREIGN KEY (operation_id) REFERENCES station_operation DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (service_id) REFERENCES station_service DEFERRABLE INITIALLY DEFERRED,
    UNIQUE (operation_id, service_id)
) STRICT;

-- region
CREATE TABLE sde.region
(
    id          INTEGER PRIMARY KEY,
    name        TEXT NOT NULL UNIQUE,
    wh_class_id INTEGER,
    faction_id  INTEGER,

    FOREIGN KEY (id) REFERENCES item_name (id) DEFERRABLE INITIALLY DEFERRED,
    CHECK (wh_class_id > 0 AND wh_class_id < 26)
    -- no fk constraint on faction_id due to reference
) STRICT;

-- constellation
CREATE TABLE sde.constellation
(
    id          INTEGER PRIMARY KEY,
    name        TEXT    NOT NULL UNIQUE,
    region_id   INTEGER NOT NULL,
    region_name TEXT    NOT NULL,

    FOREIGN KEY (id) REFERENCES item_name (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (region_id) REFERENCES region (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- solar_system_star
CREATE TABLE sde.solar_system_star
(
    id      INTEGER PRIMARY KEY,
    type_id INTEGER NOT NULL,

    FOREIGN KEY (type_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- solar_system
CREATE TABLE sde.solar_system
(
    id                 INTEGER PRIMARY KEY,
    star_id            INTEGER,
    star_type_id       INTEGER,
    name               TEXT    NOT NULL UNIQUE,
    region_name        TEXT    NOT NULL,
    region_id          INTEGER NOT NULL,
    constellation_name TEXT    NOT NULL,
    constellation_id   INTEGER NOT NULL,
    effect_type_id     INTEGER,
    security_class     TEXT,
    security           REAL,
    border             INTEGER NOT NULL,
    corridor           INTEGER NOT NULL,
    fringe             INTEGER NOT NULL,
    hub                INTEGER NOT NULL,
    international      INTEGER NOT NULL,
    regional           INTEGER NOT NULL,

    FOREIGN KEY (id) REFERENCES item_name (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (star_id) REFERENCES solar_system_star (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (region_id) REFERENCES region (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (constellation_id) REFERENCES constellation (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (effect_type_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (star_type_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED,
    CHECK (border == 1 or border == 0),
    CHECK (corridor == 1 or corridor == 0),
    CHECK (fringe == 1 or fringe == 0),
    CHECK (hub == 1 or hub == 0),
    CHECK (international == 1 or international == 0),
    CHECK (regional == 1 or regional == 0)
) STRICT;

-- solar_system_planet
CREATE TABLE sde.solar_system_planet
(
    id        INTEGER PRIMARY KEY,
    system_id INTEGER NOT NULL,
    idx       INTEGER NOT NULL,
    type_id   INTEGER NOT NULL,

    FOREIGN KEY (system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (type_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED,
    UNIQUE (system_id, idx),
    CHECK (idx > 0)
) STRICT;

-- solar_system_moon
CREATE TABLE sde.solar_system_moon
(
    id        INTEGER PRIMARY KEY,
    planet_id INTEGER NOT NULL,
    system_id INTEGER NOT NULL,
    idx       INTEGER NOT NULL,

    FOREIGN KEY (planet_id) REFERENCES solar_system_planet (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED,
    UNIQUE (planet_id, idx),
    CHECK (idx > 0)
) STRICT;

-- solar_system_asteroid_belt
CREATE TABLE sde.solar_system_asteroid_belt
(
    id        INTEGER PRIMARY KEY,
    planet_id INTEGER NOT NULL,
    system_id INTEGER NOT NULL,

    FOREIGN KEY (planet_id) REFERENCES solar_system_planet (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- stargate
CREATE TABLE sde.stargate
(
    id           INTEGER PRIMARY KEY,
    system_id    INTEGER NOT NULL,
    to_system_id INTEGER NOT NULL,

    FOREIGN KEY (system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (to_system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- npc_station
CREATE TABLE sde.npc_station
(
    id           INTEGER PRIMARY KEY,
    name         TEXT    NOT NULL,
    owner_id     INTEGER NOT NULL,
    type_id      INTEGER NOT NULL,
    operation_id INTEGER NOT NULL,
    planet_id    INTEGER NOT NULL,
    moon_id      INTEGER,
    system_id    INTEGER NOT NULL,

    FOREIGN KEY (planet_id) REFERENCES solar_system_planet (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (moon_id) REFERENCES solar_system_moon (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (operation_id) REFERENCES station_operation (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (type_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- faction
CREATE TABLE sde.faction
(
    id                     INTEGER PRIMARY KEY,
    name                   TEXT    NOT NULL,
    corporation_id         INTEGER,
    description            TEXT    NOT NULL,
    short_description      TEXT,
    icon_id                INTEGER NOT NULL,
    militia_corporation_id INTEGER,
    size_factor            REAL    NOT NULL,
    system_id              INTEGER NOT NULL,
    unique_name            INTEGER NOT NULL,

    -- no cross-referential fk constraints due to no alter table support for adding those
    -- FOREIGN KEY (corporation_id) REFERENCES npc_corporation (id),
    -- FOREIGN KEY (militia_corporation_id) REFERENCES npc_corporation (id),
    FOREIGN KEY (system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED,
    CHECK (unique_name == 1 or unique_name == 0)
) STRICT;

-- npc_corporation
CREATE table sde.npc_corporation
(
    id              INTEGER PRIMARY KEY,
    name            TEXT NOT NULL,
    ceo_id          INTEGER,
    description     TEXT,
    race_id         INTEGER,
    faction_id      INTEGER,
    icon_id         INTEGER,
    solar_system_id INTEGER,
    station_id      INTEGER,
    ticker          TEXT NOT NULL,
    unique_name     INT  NOT NULL,

    FOREIGN KEY (faction_id) REFERENCES faction (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (solar_system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (station_id) REFERENCES npc_station (id) DEFERRABLE INITIALLY DEFERRED,
    CHECK (unique_name == 1 or unique_name == 0)
) STRICT;

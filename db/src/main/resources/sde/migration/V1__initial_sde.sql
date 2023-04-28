-- ATTACH DATABASE '' AS sde;

-- dogma_attribute_category
CREATE TABLE sde.dogma_attribute_category
(
    id          INTEGER PRIMARY KEY,
    name        TEXT NOT NULL,
    description TEXT
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

    FOREIGN KEY (category_id) REFERENCES item_category (id)
) STRICT;

-- item_name
CREATE TABLE sde.item_name
(
    id       INTEGER PRIMARY KEY,
    group_id INTEGER NOT NULL,
    name     TEXT    NOT NULL,

    FOREIGN KEY (group_id) REFERENCES item_group (id)
) STRICT;

-- item_type
CREATE TABLE sde.item_type
(
    id          INTEGER PRIMARY KEY,
    name        TEXT    NOT NULL,
    group_id    INTEGER NOT NULL,
    description TEXT
) STRICT;

-- station_service
CREATE TABLE sde.station_service
(
    id   INTEGER PRIMARY KEY,
    name TEXT NOT NULL
) STRICT;

-- TODO: faction

-- region
CREATE TABLE sde.region
(
    id          INTEGER PRIMARY KEY,
    name        TEXT NOT NULL UNIQUE,
    wh_class_id INTEGER,
    faction_id  INTEGER,

    FOREIGN KEY (id) REFERENCES item_name (id),
    CHECK (wh_class_id > 0 AND wh_class_id < 26)
    -- fixme fk_faction_id
) STRICT;

-- constellation
CREATE TABLE sde.constellation
(
    id          INTEGER PRIMARY KEY,
    name        TEXT    NOT NULL UNIQUE,
    region_id   INTEGER NOT NULL,
    region_name TEXT    NOT NULL,

    FOREIGN KEY (id) REFERENCES item_name (id),
    FOREIGN KEY (region_id) REFERENCES region (id)
) STRICT;

-- solar_system_star
CREATE TABLE sde.solar_system_star
(
    id      INTEGER PRIMARY KEY,
    type_id INTEGER NOT NULL,

    FOREIGN KEY (type_id) REFERENCES item_type (id)
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

    FOREIGN KEY (id) REFERENCES item_name (id),
    FOREIGN KEY (star_id) REFERENCES solar_system_star (id),
    FOREIGN KEY (region_id) REFERENCES region (id),
    FOREIGN KEY (constellation_id) REFERENCES constellation (id),
    FOREIGN KEY (effect_type_id) REFERENCES item_type (id),
    FOREIGN KEY (star_type_id) REFERENCES item_type (id)
) STRICT;

-- solar_system_planet
CREATE TABLE sde.solar_system_planet
(
    id        INTEGER PRIMARY KEY,
    system_id INTEGER NOT NULL,
    idx       INTEGER NOT NULL,
    type_id   INTEGER NOT NULL,

    FOREIGN KEY (system_id) REFERENCES solar_system (id),
    FOREIGN KEY (type_id) REFERENCES item_type (id),
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

    FOREIGN KEY (planet_id) REFERENCES solar_system_planet (id),
    FOREIGN KEY (system_id) REFERENCES solar_system (id),
    UNIQUE (planet_id, idx),
    CHECK (idx > 0)
) STRICT;

-- solar_system_asteroid_belt
CREATE TABLE sde.solar_system_asteroid_belt
(
    id        INTEGER PRIMARY KEY,
    planet_id INTEGER NOT NULL,
    system_id INTEGER NOT NULL,

    FOREIGN KEY (planet_id) REFERENCES solar_system_planet (id),
    FOREIGN KEY (system_id) REFERENCES solar_system (id)
) STRICT;

-- stargate
CREATE TABLE sde.stargate
(

    id             INTEGER PRIMARY KEY,
    system_id      INTEGER NOT NULL,
    to_system_id   INTEGER NOT NULL,

    FOREIGN KEY (system_id) REFERENCES solar_system (id),
    FOREIGN KEY (to_system_id) REFERENCES solar_system (id)
) STRICT;

-- npc_station
CREATE TABLE sde.npc_station
(
    id        INTEGER PRIMARY KEY,
    name      TEXT    NOT NULL,
    owner_id  INTEGER NOT NULL,
    type_id   INTEGER NOT NULL,
    moon_id   INTEGER NOT NULL,
    system_id INTEGER NOT NULL,

    FOREIGN KEY (moon_id) REFERENCES solar_system_moon (id),
    FOREIGN KEY (system_id) REFERENCES solar_system (id),
    FOREIGN KEY (type_id) REFERENCES item_type (id)
) STRICT;

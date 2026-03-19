--- Changes after the SDE rework by CCP mid-2025

--- Version table changes with new SDE import format

ALTER TABLE sde.version
    ADD COLUMN released_at INTEGER NOT NULL DEFAULT 0;
ALTER TABLE sde.version
    ADD COLUMN build_number INTEGER NOT NULL DEFAULT 0;

--- item_name no longer needed
DROP TABLE sde.item_name;

--- description/short_description has been dropped
ALTER TABLE sde.faction
    DROP COLUMN description;

ALTER TABLE sde.faction
    DROP COLUMN short_description;

ALTER TABLE sde.item_type
    DROP COLUMN description;

ALTER TABLE sde.station_operation
    DROP COLUMN description;

ALTER TABLE sde.npc_corporation
    DROP COLUMN description;

--- npc_station no longer has moon/planet id directly loaded
DROP TABLE sde.npc_station;

CREATE TABLE sde.npc_station
(
    id              INTEGER PRIMARY KEY,
    name            TEXT    NOT NULL,
    owner_id        INTEGER NOT NULL,
    type_id         INTEGER NOT NULL,
    operation_id    INTEGER NOT NULL,
    orbit_id        INTEGER NOT NULL,
    orbit_index     INTEGER,
    celestial_index INTEGER,
    system_id       INTEGER NOT NULL,

    FOREIGN KEY (operation_id) REFERENCES station_operation (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (type_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

--- wormhole effects are stored explicitly now
CREATE TABLE sde.solar_system_effect
(
    id        INTEGER PRIMARY KEY,
    type_id   INTEGER NOT NULL,
    system_id INTEGER NOT NULL UNIQUE,

    FOREIGN KEY (system_id) REFERENCES solar_system (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (type_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

--- moons are not loaded any more
DROP TABLE sde.solar_system_moon;

--- planets table has a count of moons
ALTER TABLE sde.solar_system_planet
    ADD COLUMN moon_count INTEGER NOT NULL DEFAULT 0;

--- stargates have a to_system_id
ALTER TABLE sde.stargate
    ADD COLUMN to_system_id INTEGER NOT NULL DEFAULT 0 REFERENCES solar_system (id);

--- solar systems have a bunch of attributes dropped
DROP TABLE sde.solar_system;

CREATE TABLE sde.solar_system
(
    id                 INTEGER PRIMARY KEY,
    star_id            INTEGER,
    name               TEXT    NOT NULL UNIQUE,
    region_name        TEXT    NOT NULL,
    region_id          INTEGER NOT NULL,
    constellation_name TEXT    NOT NULL,
    constellation_id   INTEGER NOT NULL,
    effect_type_id     INTEGER,
    wh_class_id        INTEGER,
    security           REAL,

    FOREIGN KEY (id) REFERENCES item_name (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (star_id) REFERENCES solar_system_star (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (region_id) REFERENCES region (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (constellation_id) REFERENCES constellation (id) DEFERRABLE INITIALLY DEFERRED,
    FOREIGN KEY (effect_type_id) REFERENCES item_type (id) DEFERRABLE INITIALLY DEFERRED,
    CHECK (wh_class_id > 0 AND wh_class_id < 26)
) STRICT;

--- solar_system_star has a system_id
ALTER TABLE sde.solar_system_star
    ADD COLUMN system_id INTEGER NOT NULL DEFAULT 0 REFERENCES solar_system (id);

--- Don't see any use for this information
DROP TABLE sde.solar_system_asteroid_belt;

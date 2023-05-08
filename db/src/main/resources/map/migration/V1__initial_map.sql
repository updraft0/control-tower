-- ATTACH DATABASE '' AS map;

-- ref_wormhole_mass
CREATE TABLE map.ref_wormhole
(
    type_id           INTEGER PRIMARY KEY,
    name              TEXT    NOT NULL,
    mass_regeneration INTEGER NOT NULL,
    max_jump_mass     INTEGER NOT NULL,
    max_stable_mass   INTEGER NOT NULL,
    max_stable_time   INTEGER NOT NULL,
    target_class      INTEGER NOT NULL,

    UNIQUE (name),
    CHECK (mass_regeneration >= 0),
    CHECK (max_jump_mass > 0),
    CHECK (max_stable_mass > 0),
    CHECK (max_stable_time > 0),
    CHECK (target_class > 0)
) STRICT;

-- ref_system_static_wormhole
CREATE TABLE map.ref_system_static_wormhole
(
    system_id      INTEGER NOT NULL,
    static_type_id INTEGER NOT NULL,
    valid_from     INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT 0,
    valid_until    INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT 253402300799999, -- 9999-12-31
    updated_at     INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch()),

    FOREIGN KEY (static_type_id) REFERENCES ref_wormhole (type_id),
    UNIQUE (system_id, static_type_id, valid_from),
    CHECK (valid_until > valid_from)
) STRICT;
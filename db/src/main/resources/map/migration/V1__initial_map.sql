-- ATTACH DATABASE '' AS map;

-- map
CREATE TABLE map.map
(
    id                 INTEGER PRIMARY KEY,
    name               TEXT    NOT NULL,
    created_at         INTEGER NOT NULL DEFAULT (unixepoch() * 1000),
    creator_user_id    INTEGER NOT NULL,
    display_type       INTEGER NOT NULL DEFAULT 0,
    deleted_at         INTEGER,
    deleted_by_user_id INTEGER,

    UNIQUE (name COLLATE NOCASE),
    CHECK (name <> '')
) STRICT;

-- map_system
CREATE TABLE map.map_system
(
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    system_id               INTEGER NOT NULL,
    name                    TEXT,
    is_pinned               INTEGER NOT NULL                     DEFAULT 0,
    chain_naming_strategy   INTEGER NOT NULL                     DEFAULT 0,
    description             TEXT,
    stance                  INTEGER NOT NULL                     DEFAULT 0,

    updated_by_character_id INTEGER NOT NULL,
    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),

    UNIQUE (map_id, system_id),
    CHECK (name is null or length(name) > 0),
    CHECK (is_pinned == 0 or is_pinned == 1),
    CHECK (stance >= 0 or stance < 3)
) STRICT;

-- map_system_display
CREATE TABLE map.map_system_display
(
    map_id       INTEGER NOT NULL REFERENCES map (id),
    system_id    INTEGER NOT NULL,
    display_type INTEGER NOT NULL,
    data         TEXT    NOT NULL,

    updated_at   INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),

    UNIQUE (map_id, system_id),
    FOREIGN KEY (map_id, system_id) REFERENCES map_system (map_id, system_id)
) STRICT;

-- map_system_structure
CREATE TABLE map.map_system_structure
(
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    system_id               INTEGER NOT NULL,
    name                    TEXT    NOT NULL,
    is_deleted              INTEGER NOT NULL                     DEFAULT 0,
    owner_corporation_id    INTEGER REFERENCES corporation (id),
    structure_type          TEXT,
    location                TEXT,

    created_at              INTEGER NOT NULL                     DEFAULT (unixepoch() * 1000),
    created_by_character_id INTEGER NOT NULL,
    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    updated_by_character_id INTEGER NOT NULL,

    UNIQUE (map_id, system_id, name),
    FOREIGN KEY (map_id, system_id) REFERENCES map_system (map_id, system_id),
    CHECK (is_deleted == 0 or is_deleted == 1),
    CHECK (length(structure_type) > 2)
) STRICT;

-- map_system_note
CREATE TABLE map.map_system_note
(
    id                      INTEGER PRIMARY KEY,
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    system_id               INTEGER NOT NULL,
    note                    TEXT    NOT NULL,
    is_deleted              INTEGER NOT NULL                     DEFAULT 0,

    created_at              INTEGER NOT NULL                     DEFAULT (unixepoch() * 1000),
    created_by_character_id INTEGER NOT NULL,
    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    updated_by_character_id INTEGER NOT NULL,

    FOREIGN KEY (map_id, system_id) REFERENCES map_system (map_id, system_id),
    CHECK (is_deleted == 0 or is_deleted == 1),
    CHECK (length(note) > 0)
) STRICT;

-- map_wormhole_connection
CREATE TABLE map.map_wormhole_connection
(
    id                      INTEGER PRIMARY KEY,
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    from_system_id          INTEGER NOT NULL,
    to_system_id            INTEGER NOT NULL,
    is_deleted              INTEGER NOT NULL                     DEFAULT 0,

    created_at              INTEGER NOT NULL                     DEFAULT (unixepoch() * 1000),
    created_by_character_id INTEGER NOT NULL,
    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    updated_by_character_id INTEGER NOT NULL,

    FOREIGN KEY (map_id, from_system_id) REFERENCES map_system (map_id, system_id),
    FOREIGN KEY (map_id, to_system_id) REFERENCES map_system (map_id, system_id),
    CHECK (is_deleted == 0 or is_deleted == 1)
) STRICT;

-- map_system_signature
CREATE TABLE map.map_system_signature
(
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    system_id               INTEGER NOT NULL,
    signature_id            TEXT    NOT NULL,
    is_deleted              INTEGER NOT NULL                     DEFAULT 0,
    signature_group         INTEGER NOT NULL                     DEFAULT 0,
    signature_type_name     TEXT,
    wormhole_is_eol         INTEGER,
    wormhole_eol_at         INTEGER,
    wormhole_type_id        INTEGER REFERENCES ref_wormhole (type_id),
    wormhole_mass_size      INTEGER                              DEFAULT 0,
    wormhole_mass_status    INTEGER                              DEFAULT 0,
    wormhole_k162_type      INTEGER,
    wormhole_connection_id  INTEGER REFERENCES map_wormhole_connection (id),

    created_at              INTEGER NOT NULL                     DEFAULT (unixepoch() * 1000),
    created_by_character_id INTEGER NOT NULL,
    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    updated_by_character_id INTEGER NOT NULL,
    expires_at              INTEGER NOT NULL                     DEFAULT ((unixepoch() + 172800 /* 2 days */) * 1000),

    UNIQUE (map_id, system_id, signature_id),
    FOREIGN KEY (map_id, system_id) REFERENCES map_system (map_id, system_id),
    CHECK (length(signature_id) == 7),
    CHECK (is_deleted == 0 or is_deleted == 1),
    CHECK (signature_group >= 0 and signature_group < 8),
    CHECK (wormhole_k162_type is null or (wormhole_k162_type >= 0 and wormhole_k162_type < 7)),
    CHECK (wormhole_is_eol is null or wormhole_is_eol == 0 or wormhole_is_eol == 1),
    CHECK (wormhole_mass_status is null or (wormhole_mass_status >= 0 and wormhole_mass_status < 4))
) STRICT;

-- alliance
CREATE TABLE map.alliance
(
    id                      INTEGER PRIMARY KEY,
    name                    TEXT    NOT NULL,
    ticker                  TEXT    NOT NULL,
    creator_character_id    INTEGER NOT NULL,
    creator_corporation_id  INTEGER NOT NULL REFERENCES corporation (id),
    executor_corporation_id INTEGER NOT NULL REFERENCES corporation (id),
    created_at              INTEGER NOT NULL                     DEFAULT (unixepoch() * 1000),
    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),

    UNIQUE (name)
) STRICT;

-- corporation
CREATE TABLE map.corporation
(
    id                   INTEGER PRIMARY KEY,
    name                 TEXT    NOT NULL,
    ticker               TEXT    NOT NULL,
    alliance_id          INTEGER REFERENCES alliance (id),
    ceo_character_id     INTEGER NOT NULL,
    creator_character_id INTEGER NOT NULL,
    home_station_id      INTEGER NOT NULL,
    member_count         INTEGER NOT NULL,
    url                  TEXT,
    created_at           INTEGER NOT NULL,
    updated_at           INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),

    UNIQUE (name),
    CHECK (member_count > 0)
) STRICT;

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
    static_type_id INTEGER NOT NULL REFERENCES ref_wormhole (type_id),
    valid_from     INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT 0,
    valid_until    INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT 253402300799999, -- 9999-12-31 23:59:59.999
    updated_at     INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),

    UNIQUE (system_id, static_type_id, valid_from),
    CHECK (valid_until > valid_from)
) STRICT;

-- ref_signature_in_group
CREATE TABLE map.ref_signature_in_group
(
    signature_group INTEGER NOT NULL,
    name            TEXT    NOT NULL,
    target_classes  INTEGER NOT NULL,

    UNIQUE (signature_group, name),
    CHECK (signature_group >= 0 and signature_group < 8),
    CHECK (length(name) > 3)
) STRICT;
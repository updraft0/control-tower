-- remove tables not to be used
DROP TABLE IF EXISTS map.map_system_note;
DROP TABLE IF EXISTS map.map_system_structure;

-- intel_system_structure
CREATE TABLE map.intel_system_structure
(
    id                      INTEGER PRIMARY KEY,
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    system_id               INTEGER NOT NULL,
    name                    TEXT,
    owner_corporation_id    INTEGER REFERENCES corporation (id),
    item_type_id            INTEGER NOT NULL,
    nearest_planet_idx      INTEGER,
    nearest_moon_idx        INTEGER,
    is_online               INTEGER,
    is_deleted              INTEGER NOT NULL                     DEFAULT 0,

    created_at              INTEGER NOT NULL                     DEFAULT (unixepoch() * 1000),
    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    deleted_at              INTEGER,

    created_by_character_id INTEGER NOT NULL,
    updated_by_character_id INTEGER NOT NULL,
    deleted_by_character_id INTEGER,

    CHECK (name is null or length(name) > 0),
    CHECK (is_online is null or is_online is 0 or is_online is 1),
    CHECK (is_deleted is 0 or (is_deleted is 1 and deleted_at is not null and deleted_by_character_id is not null))
) STRICT;

-- intel_system
CREATE TABLE map.intel_system
(
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    system_id               INTEGER NOT NULL,

    primary_corporation_id  INTEGER REFERENCES corporation (id),
    primary_alliance_id     INTEGER REFERENCES alliance (id),

    intel_group             INTEGER NOT NULL                     DEFAULT 0,
    is_empty                INTEGER NOT NULL                     DEFAULT 0,

    created_at              INTEGER NOT NULL                     DEFAULT (unixepoch() * 1000),
    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    created_by_character_id INTEGER NOT NULL,
    updated_by_character_id INTEGER NOT NULL,

    UNIQUE (map_id, system_id),

    CHECK (intel_group < 0 and intel_group < 4),
    CHECK (is_empty is 0 or is_empty is 1)
) STRICT;

-- intel_system_note
CREATE TABLE map.intel_system_note
(
    id                      INTEGER PRIMARY KEY,
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    system_id               INTEGER NOT NULL,
    note                    TEXT    NOT NULL,

    is_pinned               INTEGER NOT NULL DEFAULT 0,
    is_deleted              INTEGER NOT NULL DEFAULT 0,
    original_id             INTEGER REFERENCES intel_system_note (id),

    created_at              INTEGER NOT NULL DEFAULT (unixepoch() * 1000),
    created_by_character_id INTEGER NOT NULL,

    deleted_at              INTEGER,
    deleted_by_character_id INTEGER,

    CHECK (length(note) > 0),
    CHECK (is_deleted is 0 or (is_deleted is 1 and deleted_at is not null and deleted_by_character_id is not null))
) STRICT;

-- intel_system_ping
CREATE TABLE map.intel_system_ping
(
    id                      INTEGER PRIMARY KEY,
    map_id                  INTEGER NOT NULL REFERENCES map (id),
    system_id               INTEGER NOT NULL,

    ping_user_id            INTEGER,
    ping_map_global         INTEGER,
    ping_note               TEXT,
    is_deleted              INTEGER NOT NULL DEFAULT 0,

    created_at              INTEGER NOT NULL DEFAULT (unixepoch() * 1000),
    created_by_character_id INTEGER NOT NULL,

    deleted_at              INTEGER,
    deleted_by_character_id INTEGER,

    CHECK (is_deleted is 0 or (is_deleted is 1 and deleted_at is not null and deleted_by_character_id is not null)),
    CHECK (ping_user_id is not null or ping_map_global is not null),
    CHECK (ping_map_global is null or ping_map_global is 1)
) STRICT;

-- intel_group_stance
CREATE TABLE map.intel_group_stance
(
    map_id                  INTEGER NOT NULL REFERENCES map (id),

    corporation_id          INTEGER REFERENCES corporation (id),
    alliance_id             INTEGER REFERENCES alliance (id),
    stance                  INTEGER NOT NULL,

    created_at              INTEGER NOT NULL                     DEFAULT (unixepoch() * 1000),
    created_by_character_id INTEGER NOT NULL,

    updated_at              INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    updated_by_character_id INTEGER NOT NULL,

    UNIQUE (map_id, corporation_id, alliance_id),
    CHECK (corporation_id is not null or alliance_id is not null),
    CHECK (stance >= 0 or stance < 3)
) STRICT;

-- intel_character
CREATE TABLE map.intel_character
(
    id              INTEGER PRIMARY KEY,
    bloodline_id    INTEGER NOT NULL,
    corporation_id  INTEGER NOT NULL,
    faction_id      INTEGER,
    gender          TEXT    NOT NULL,
    name            TEXT    NOT NULL,
    race_id         INTEGER NOT NULL,
    security_status REAL,
    title           TEXT,

    created_at      INTEGER NOT NULL,
    updated_at      INTEGER NOT NULL,

    UNIQUE (name)
) STRICT;

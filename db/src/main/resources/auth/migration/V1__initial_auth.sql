-- ATTACH DATABASE '' AS auth;

-- character
CREATE TABLE auth.character
(
    owner_hash     TEXT PRIMARY KEY,
    id             INTEGER NOT NULL,
    name           TEXT    NOT NULL,
    corporation_id INTEGER NOT NULL,
    alliance_id    INTEGER,
    born_at        INTEGER NOT NULL,
    added_at       INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    updated_at     INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    last_online_at INTEGER,

    UNIQUE (id),
    UNIQUE (name)
) STRICT;

-- character_auth_token
CREATE TABLE auth.character_auth_token
(
    character_id  INTEGER PRIMARY KEY,
    nonce         TEXT    NOT NULL,
    token         TEXT    NOT NULL,
    refresh_token TEXT    NOT NULL,
    expires_at    INTEGER NOT NULL,
    updated_at    INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000)
) STRICT;

-- user
CREATE TABLE auth.user
(
    id           INTEGER PRIMARY KEY,
    display_name TEXT    NOT NULL,
    created_at   INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),

    UNIQUE (display_name)
) STRICT;

-- user_character
CREATE TABLE auth.user_character
(
    user_id      INTEGER NOT NULL REFERENCES user (id) DEFERRABLE INITIALLY DEFERRED,
    character_id INTEGER NOT NULL REFERENCES character (id) DEFERRABLE INITIALLY DEFERRED
) STRICT;

-- user_session
CREATE TABLE auth.user_session
(
    session_id   TEXT    NOT NULL,
    user_id      INTEGER REFERENCES user (id) DEFERRABLE INITIALLY DEFERRED,
    created_at   INTEGER NOT NULL,
    expires_at   INTEGER NOT NULL,
    last_seen_at INTEGER,
    ip_address   TEXT,
    user_agent   TEXT,

    UNIQUE (session_id, user_id)
) STRICT;

-- map_policy
CREATE TABLE auth.map_policy
(
    map_id             INTEGER PRIMARY KEY,

    created_by_user_id INTEGER NOT NULL REFERENCES user (id) DEFERRABLE INITIALLY DEFERRED,
    created_at         INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000)
) STRICT;

-- map_policy_member
CREATE TABLE auth.map_policy_member
(
    id                 INTEGER PRIMARY KEY,
    map_id             INTEGER NOT NULL,
    member_id          INTEGER NOT NULL,
    member_type        TEXT    NOT NULL,
    is_deny            INTEGER NOT NULL                     DEFAULT 0,
    role               TEXT    NOT NULL,

    created_by_user_id INTEGER NOT NULL REFERENCES user (id) DEFERRABLE INITIALLY DEFERRED,
    created_at         INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),
    updated_by_user_id INTEGER NOT NULL REFERENCES user (id) DEFERRABLE INITIALLY DEFERRED,
    updated_at         INTEGER NOT NULL ON CONFLICT REPLACE DEFAULT (unixepoch() * 1000),

    UNIQUE (map_id, member_id, member_type, is_deny, role),

    CHECK (member_type == 'character' or member_type == 'corporation' or member_type == 'alliance'),
    CHECK (role == 'viewer' or role == 'editor' or role == 'admin'),
    CHECK (is_deny == 0 or is_deny == 1)
) STRICT;
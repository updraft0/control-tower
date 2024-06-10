-- map_wormhole_connection_jump
CREATE TABLE map.map_wormhole_connection_jump
(
    connection_id INTEGER NOT NULL REFERENCES map_wormhole_connection (id),
    character_id  INTEGER NOT NULL,
    ship_type_id  INTEGER NOT NULL,
    mass_override INTEGER,
    created_at    INTEGER NOT NULL DEFAULT (unixepoch() * 1000)
) STRICT;
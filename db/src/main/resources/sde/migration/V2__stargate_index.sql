-- create index on system_id and to_system_id to speed up lookups
CREATE INDEX sde.idx_stargate_system_id ON stargate (system_id);
CREATE INDEX sde.idx_stargate_to_system_id ON stargate (to_system_id);

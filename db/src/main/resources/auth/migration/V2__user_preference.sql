-- user_preference
CREATE TABLE auth.user_preference
(
    user_id             INTEGER REFERENCES user (id) DEFERRABLE INITIALLY DEFERRED,
    preference_json     TEXT NOT NULL,

    UNIQUE (user_id)
) STRICT;

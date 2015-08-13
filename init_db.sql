BEGIN; 

DROP TABLE IF EXISTS "images" CASCADE;
DROP TABLE IF EXISTS "entries" CASCADE;
DROP TABLE IF EXISTS "people" CASCADE;
DROP TABLE IF EXISTS "trips" CASCADE;
DROP TABLE IF EXISTS "sources" CASCADE;
DROP TABLE IF EXISTS "reasons" CASCADE;

DROP TYPE IF EXISTS img_type;

CREATE TABLE "reasons" (
    id     SERIAL       NOT NULL,
    reason VARCHAR(160) NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE "trips" (
    id        SERIAL  NOT NULL,
    reason_id INTEGER NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (reason_id) REFERENCES "reasons" (id) ON DELETE CASCADE
);

CREATE TABLE "sources" (
    id     SERIAL      NOT NULL,
    source VARCHAR(25) NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE "people" (
    id           SERIAL       NOT NULL,
    name         VARCHAR(160) NOT NULL,
    nickname     VARCHAR(25),
    csh_username VARCHAR(25),
    source       INTEGER      NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE "entries" (
    id         SERIAL  NOT NULL,
    person_id  INTEGER NOT NULL,
    trip_id    INTEGER NOT NULL,
    date_start DATE    NOT NULL,
    date_end   DATE    NOT NULL,
    entry      TEXT    NOT NULL,
    image_path TEXT,
    PRIMARY KEY (id),
    FOREIGN KEY (person_id) REFERENCES "people" (id) ON DELETE CASCADE,
    FOREIGN KEY (trip_id) REFERENCES "trips" (id) ON DELETE CASCADE
);

CREATE TYPE img_type AS ENUM ('entry', 'doodle');

CREATE TABLE "images" (
    id         SERIAL   NOT NULL,
    entry_id   INTEGER  NOT NULL,
    image_path TEXT     NOT NULL,
    img_type   img_type NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (entry_id) REFERENCES "entries" (id) ON DELETE CASCADE
);

COMMIT;

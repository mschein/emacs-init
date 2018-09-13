--
-- can I do a better check for validity?
--
-- Table:
--
--  Metadata:
--     cluster_timeout
--     task_timeout
--
--  Clusters:
--     id
--     cluster-name
--     insert-time
--
--  Tasks:
--    id
--    task-name
--    cluster-id (foreign-key: cluster id)
--    insert-time
--
--  Services:
--    id
--    service-name
--
-- SELECT id from tasks
--   where
--
-- Basic ideas
--
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS clusters (
       id integer primary key,
       cluster_name TEXT unique not null, -- index me
       insert_time TEXT
);
CREATE UNIQUE INDEX IF NOT EXISTS clusters_cluster_name ON clusters(cluster_name);

CREATE TRIGGER IF NOT EXISTS cluster_insert_trigger
   AFTER INSERT
   ON clusters
   BEGIN
        UPDATE clusters
        SET insert_time = datetime('now')
        where id = new.id;
   END;


CREATE TABLE IF NOT EXISTS tasks (
       id integer primary key,
       task_name TEXT unique not null,
       cluster_id integer,
       insert_time TEXT,
       FOREIGN KEY (cluster_id) REFERENCES clusters(id) ON DELETE CASCADE
);
CREATE UNIQUE INDEX IF NOT EXISTS tasks_task_name ON tasks(task_name);
CREATE TRIGGER IF NOT EXISTS tasks_insert_trigger
   AFTER INSERT
   ON tasks
   BEGIN
        UPDATE tasks
        SET insert_time = datetime('now')
        where id = new.id;
   END;


CREATE TABLE IF NOT EXISTS instances (
       id integer primary key,
       instance_name TEXT unique not null, -- index me
       ip TEXT,
       cluster_id integer,
       insert_time TEXT,
       FOREIGN KEY (cluster_id) REFERENCES clusters(id) ON DELETE CASCADE
);
CREATE UNIQUE INDEX IF NOT EXISTS instances_instance_name ON instances(instance_name);
CREATE UNIQUE INDEX IF NOT EXISTS instances_ip ON instances(ip);
CREATE TRIGGER IF NOT EXISTS instances_insert_trigger
   AFTER INSERT
   ON instances
   BEGIN
        UPDATE instances
        SET insert_time = datetime('now')
        where id = new.id;
   END;


CREATE TABLE IF NOT EXISTS services (
       id integer primary key,
       service_name TEXT unique not null, -- index me
       cluster_id integer,
       insert_time TEXT,
       FOREIGN KEY (cluster_id) REFERENCES clusters(id) ON DELETE CASCADE
);
CREATE UNIQUE INDEX IF NOT EXISTS services_service_name ON services(service_name);
CREATE TRIGGER IF NOT EXISTS services_insert_trigger
   AFTER INSERT
   ON services
   BEGIN
        UPDATE services
        SET insert_time = datetime('now')
        where id = new.id;
   END;


CREATE TABLE IF NOT EXISTS service_instance (
       instance_id integer not null,
       service_id integer not null,
       PRIMARY KEY (instance_id, service_id),
       FOREIGN KEY (instance_id) REFERENCES instances(id) ON DELETE CASCADE,
       FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE
);


CREATE TABLE IF NOT EXISTS task_instance (
       instance_id integer not null,
       task_id integer not null,
       PRIMARY KEY (instance_id, task_id),
       FOREIGN KEY (instance_id) REFERENCES instances(id) ON DELETE CASCADE,
       FOREIGN KEY (task_id) REFERENCES task(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS service_task (
       task_id integer not null,
       service_id integer not null,
       PRIMARY KEY (task_id, service_id),
       FOREIGN KEY (task_id) REFERENCES task(id) ON DELETE CASCADE,
       FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE
);

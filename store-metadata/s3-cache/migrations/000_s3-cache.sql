-- Initial database creation for store s3-cache

CREATE TABLE s3_cache (
	bucket TEXT NOT NULL,
	"key" TEXT NOT NULL,
	etag TEXT NOT NULL UNIQUE,
	data TEXT NOT NULL,
        last_ping DATETIME,
	PRIMARY KEY (bucket, "key")
);

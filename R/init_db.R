#' Create Database Schema
#' @param con A DBI connection object
#' @export
create_schema <- function(con) {
  # 1. Enable Foreign Key support for this connection
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")

  # 2. Create Classes Table
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ont_classes (
    class_id TEXT PRIMARY KEY,
    parent_class_id TEXT,
    description TEXT,
    FOREIGN KEY (parent_class_id) REFERENCES ont_classes(class_id)
  );")

  # 3. Create Properties Table
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ont_properties (
    property_id TEXT PRIMARY KEY,
    value_type TEXT,
    description TEXT
  );")

  # 4. Create Instances Table
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS instances (
    instance_id TEXT PRIMARY KEY,
    class_id TEXT,
    label TEXT,
    checksum TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    created_by TEXT,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_by TEXT,
    FOREIGN KEY (class_id) REFERENCES ont_classes(class_id)
  );")

  DBI::dbExecute(con, "CREATE TRIGGER IF NOT EXISTS trg_instances_updated_at
    AFTER UPDATE ON instances
    FOR EACH ROW
      BEGIN
        UPDATE instances SET updated_at = CURRENT_TIMESTAMP WHERE instance_id = OLD.instance_id;
      END;")

  # 5. Create Relations Table (IMPORTANT: Added Foreign Keys)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS relations (
    subject_id TEXT,
    predicate_id TEXT,
    object_id TEXT,
    sort_order INTEGER,
    PRIMARY KEY (subject_id, predicate_id, object_id),
    FOREIGN KEY (subject_id) REFERENCES instances(instance_id) ON DELETE CASCADE
  );")

  # 6. Create Attributes Table (IMPORTANT: Added Foreign Keys)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS attributes (
    instance_id TEXT,
    property_id TEXT,
    value TEXT,
    PRIMARY KEY (instance_id, property_id),
    FOREIGN KEY (instance_id) REFERENCES instances(instance_id) ON DELETE CASCADE
  );")

  # 7. Create Constraints Table
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ont_constraints (
    parent_class_id TEXT,
    required_child_class_id TEXT
  );")

  # 8. Mandatory Seeds for tests and system operation
  classes <- c("Entity", "Document", "Section", "StandardClause", "MandatorySection", "Conclusion")
  for (cls in classes) {
    DBI::dbExecute(con, "INSERT OR IGNORE INTO ont_classes (class_id) VALUES (?)", params = list(cls))
  }

  props <- c("content", "version", "status", "author", "table_data", "plot_path")
  for (prp in props) {
    DBI::dbExecute(con, "INSERT OR IGNORE INTO ont_properties (property_id, value_type) VALUES (?, 'text')", params = list(prp))
  }

  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS signatures (
    signature_id INTEGER PRIMARY KEY AUTOINCREMENT,
    instance_id TEXT,
    version TEXT,
    user_id TEXT,
    role TEXT,
    meaning TEXT,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    content_hash TEXT,    -- <--- НОВАЯ КОЛОНКА ДЛЯ ХЕША
    FOREIGN KEY (instance_id) REFERENCES instances(instance_id)
  );")

  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS audit_log (
    log_id INTEGER PRIMARY KEY AUTOINCREMENT,
    instance_id TEXT,
    user_id TEXT,
    action TEXT,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (instance_id) REFERENCES instances(instance_id)
  );")

  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ont_consistency_rules (
    property_id TEXT PRIMARY KEY,
    severity TEXT -- 'ERROR' или 'WARNING'
  );")

  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ont_relation_rules (
    subject_class TEXT,
    predicate_id TEXT,
    object_class TEXT
  )")
}

parser grammar MySQLParser;

options { tokenVocab = MySQLLexer; }

// expression statement -------  http://dev.mysql.com/doc/refman/5.6/en/expressions.html  -------------
expression:	exp_factor1 ( OR_SYM exp_factor1 )* ;
exp_factor1:	exp_factor2 ( XOR exp_factor2 )* ;
exp_factor2:	exp_factor3 ( AND_SYM exp_factor3 )* ;
exp_factor3:	(NOT_SYM)? exp_factor4 ;
exp_factor4:	bool_primary ( IS_SYM (NOT_SYM)? (boolean_literal|NULL_SYM) )? ;
bool_primary:
	  ( predicate relational_op predicate )
	| ( predicate relational_op ( ALL | ANY )? subquery )
	| ( NOT_SYM? EXISTS subquery )
	| predicate
;
predicate:
	  ( bit_expr (NOT_SYM)? IN_SYM (subquery | expression_list) )
	| ( bit_expr (NOT_SYM)? BETWEEN bit_expr AND_SYM predicate )
	| ( bit_expr SOUNDS_SYM LIKE_SYM bit_expr )
	| ( bit_expr (NOT_SYM)? LIKE_SYM simple_expr (ESCAPE_SYM simple_expr)? )
	| ( bit_expr (NOT_SYM)? REGEXP bit_expr )
	| ( bit_expr )
;
bit_expr:
	factor1 ( VERTBAR factor1 )? ;
factor1:
	factor2 ( BITAND factor2 )? ;
factor2:
	factor3 ( (SHIFT_LEFT|SHIFT_RIGHT) factor3 )? ;
factor3:
	factor4 ( (PLUS|MINUS) factor4 )? ;
factor4:
	factor5 ( (ASTERISK|DIVIDE|MOD_SYM|POWER_OP) factor5 )? ;
factor5:
	factor6 ( (PLUS|MINUS) interval_expr )? ;
factor6:
	(PLUS | MINUS | NEGATION | BINARY) simple_expr
	| simple_expr ;
factor7:
	simple_expr (COLLATE_SYM collation_names)?;
simple_expr:
	literal_value
	| column_spec
	| function_call
	//| param_marker
	| USER_VAR
	| expression_list
	| (ROW_SYM expression_list)
	| subquery
	| EXISTS subquery
	//| {identifier expression}
	| match_against_statement
	| case_when_statement
	| interval_expr
;


function_call:
	  (  functionList ( LPAREN (expression (COMMA expression)*)? RPAREN ) ?  )
	| (  CAST_SYM LPAREN expression AS_SYM cast_data_type RPAREN  )
	| (  CONVERT_SYM LPAREN expression COMMA cast_data_type RPAREN  )
	| (  CONVERT_SYM LPAREN expression USING_SYM transcoding_name RPAREN  )
	| (  group_functions LPAREN ( ASTERISK | ALL | DISTINCT )? bit_expr RPAREN  )
;

case_when_statement:
        case_when_statement1 | case_when_statement2
;
case_when_statement1:
        CASE_SYM
        ( WHEN_SYM expression THEN_SYM bit_expr )+
        ( ELSE_SYM bit_expr )?
        END_SYM
;
case_when_statement2:
        CASE_SYM bit_expr
        ( WHEN_SYM bit_expr THEN_SYM bit_expr )+
        ( ELSE_SYM bit_expr )?
        END_SYM
;

match_against_statement:
	MATCH (column_spec (COMMA column_spec)* ) AGAINST (expression (search_modifier)? )
;

column_spec:
	( ( schema_name DOT )? table_name DOT )? column_name ;

expression_list:
	LPAREN expression ( COMMA expression )* RPAREN ;

interval_expr:
	INTERVAL_SYM expression interval_unit
;






// JOIN Syntax ----------  http://dev.mysql.com/doc/refman/5.6/en/join.html  ---------------
table_references:
        table_reference ( COMMA table_reference )*
;
table_reference:
	table_factor1 | table_atom
;
table_factor1:
	table_factor2 (  (INNER_SYM | CROSS)? JOIN_SYM table_atom (join_condition)?  )?
;
table_factor2:
	table_factor3 (  STRAIGHT_JOIN table_atom (ON expression)?  )?
;
table_factor3:
	table_factor4 (  (LEFT|RIGHT) (OUTER)? JOIN_SYM table_factor4 join_condition  )?
;
table_factor4:
	table_atom (  NATURAL ( (LEFT|RIGHT) (OUTER)? )? JOIN_SYM table_atom )?
;
table_atom:
	  ( table_spec (partition_clause)? (alias)? (index_hint_list)? )
	| ( subquery alias )
	| ( LPAREN table_references RPAREN )
	| ( OJ_SYM table_reference LEFT OUTER JOIN_SYM table_reference ON expression )
;
join_condition:
	  (ON expression) | (USING_SYM column_list)
;
index_hint_list:
	index_hint (COMMA index_hint)*
;
index_options:
	(INDEX_SYM | KEY_SYM) (  FOR_SYM ((JOIN_SYM) | (ORDER_SYM BY_SYM) | (GROUP_SYM BY_SYM))  )?
;
index_hint:
	  USE_SYM    index_options LPAREN (index_list)? RPAREN
	| IGNORE_SYM index_options LPAREN index_list RPAREN
	| FORCE_SYM  index_options LPAREN index_list RPAREN
;
index_list:
	index_name (COMMA index_name)*
;
partition_clause:
	PARTITION_SYM LPAREN partition_names RPAREN
;
partition_names:	partition_name (COMMA partition_name)* ;






// SQL Statement Syntax ----  http://dev.mysql.com/doc/refman/5.6/en/sql-syntax.html ----------
root_statement:
	(SHIFT_LEFT SHIFT_RIGHT)?
	( data_manipulation_statements | data_definition_statements /*| transactional_locking_statements | replication_statements*/ )
	(SEMI)?
;

data_manipulation_statements:
	  select_statement
	| delete_statements
	| insert_statements
	| update_statements

	| call_statement
	| do_statement
	| handler_statements
	| load_data_statement
	| load_xml_statement
	| replace_statement
;

data_definition_statements:
	  create_database_statement
	| alter_database_statements
	| drop_database_statement

	| create_event_statement
	| alter_event_statement
	| drop_event_statement

	//| create_function_statement
	//| alter_function_statement
	//| drop_function_statement

	//| create_procedure_create_function_statement
	//| alter_procedure_statement
	//| drop_procedure_drop_function_statement

	//| create_trigger_statement
	//| drop_trigger_statement

	| create_server_statement
	| alter_server_statement
	| drop_server_statement

	| create_table_statement
	| alter_table_statement
	| drop_table_statement

	| create_view_statement
	| alter_view_statement
	| rename_table_statement
	| drop_view_statement
	| truncate_table_statement

	| create_index_statement
	| drop_index_statement
;

/*transactional_locking_statements:
	  start_transaction_statement
	| comment_statement
	| rollback_statement

	| savepoint_statement
	| rollback_to_savepoint_statement
	| release_savepoint_statement

	| lock_table_statement
	| unlock_table_statement

	| set_transaction_statement

	| xa_transaction_statement
;

replication_statements:
	  controlling_master_servers_statements
	| controlling_slave_servers_statements
;
*/







// select ------  http://dev.mysql.com/doc/refman/5.6/en/select.html  -------------------------------
select_statement:
        select_expression ( (UNION_SYM (ALL)?) select_expression )*
;

select_expression:
	SELECT

	( ALL | DISTINCT | DISTINCTROW )?
	(HIGH_PRIORITY)?
	(STRAIGHT_JOIN)?
	(SQL_SMALL_RESULT)? (SQL_BIG_RESULT)? (SQL_BUFFER_RESULT)?
	(SQL_CACHE_SYM | SQL_NO_CACHE_SYM)? (SQL_CALC_FOUND_ROWS)?

	select_list

	(
		FROM table_references
		( partition_clause )?
		( where_clause )?
		( groupby_clause )?
		( having_clause )?
	) ?

	( orderby_clause )?
	( limit_clause )?
	( ( FOR_SYM UPDATE) | (LOCK IN_SYM SHARE_SYM MODE_SYM) )?
;

where_clause:
	WHERE expression
;

groupby_clause:
	GROUP_SYM BY_SYM groupby_item (COMMA groupby_item)* (WITH ROLLUP_SYM)?
;
groupby_item:	column_spec | INTEGER_NUM | bit_expr ;

having_clause:
	HAVING expression
;

orderby_clause:
	ORDER_SYM BY_SYM orderby_item (COMMA orderby_item)*
;
orderby_item:	groupby_item (ASC | DESC)? ;

limit_clause:
	LIMIT ((offset COMMA)? row_count) | (row_count OFFSET_SYM offset)
;
offset:		INTEGER_NUM ;
row_count:	INTEGER_NUM ;

select_list:
	( ( displayed_column ( COMMA displayed_column )*)
	| ASTERISK )
;

column_list:
	LPAREN column_spec (COMMA column_spec)* RPAREN
;

subquery:
	LPAREN select_statement RPAREN
;

table_spec:
	( schema_name DOT )? table_name
;

displayed_column :
	( table_spec DOT ASTERISK )
	|
	( column_spec (alias)? )
	|
	( bit_expr (alias)? )
;







// delete ------  http://dev.mysql.com/doc/refman/5.6/en/delete.html  ------------------------
delete_statements:
	DELETE_SYM (LOW_PRIORITY)? (QUICK)? (IGNORE_SYM)?
	( delete_single_table_statement | delete_multiple_table_statement1 | delete_multiple_table_statement2 )
;
delete_single_table_statement:
	FROM table_spec
	(partition_clause)?
	(where_clause)?
	(orderby_clause)?
	(limit_clause)?
;
delete_multiple_table_statement1:
	table_spec (ALL_FIELDS)? (COMMA table_spec (ALL_FIELDS)?)*
	FROM table_references
	(where_clause)?
;
delete_multiple_table_statement2:
	FROM table_spec (ALL_FIELDS)? (COMMA table_spec (ALL_FIELDS)?)*
	USING_SYM table_references
	(where_clause)?
;





// insert ---------  http://dev.mysql.com/doc/refman/5.6/en/insert.html  -------------------------
insert_statements :
	insert_statement1 | insert_statement2 | insert_statement3
;

insert_header:
	INSERT (LOW_PRIORITY | HIGH_PRIORITY)? (IGNORE_SYM)?
	(INTO)? table_spec
	(partition_clause)?
;

insert_subfix:
	ON DUPLICATE_SYM KEY_SYM UPDATE column_spec EQ_SYM expression (COMMA column_spec EQ_SYM expression)*
;

insert_statement1:
	insert_header
	(column_list)?
	value_list_clause
	( insert_subfix )?
;
value_list_clause:	(VALUES | VALUE_SYM) column_value_list (COMMA column_value_list)*;
column_value_list:	LPAREN (bit_expr|DEFAULT) (COMMA (bit_expr|DEFAULT) )* RPAREN ;

insert_statement2:
	insert_header
	set_columns_cluase
	( insert_subfix )?
;
set_columns_cluase:	SET_SYM set_column_cluase ( COMMA set_column_cluase )*;
set_column_cluase:	column_spec EQ_SYM (expression|DEFAULT) ;

insert_statement3:
	insert_header
	(column_list)?
	select_expression
	( insert_subfix )?
;







// update --------  http://dev.mysql.com/doc/refman/5.6/en/update.html  ------------------------
update_statements :
	single_table_update_statement | multiple_table_update_statement
;

single_table_update_statement:
UPDATE (LOW_PRIORITY)? (IGNORE_SYM)? table_reference
	set_columns_cluase
	(where_clause)?
	(orderby_clause)?
	(limit_clause)?
;

multiple_table_update_statement:
	UPDATE (LOW_PRIORITY)? (IGNORE_SYM)? table_references
	set_columns_cluase
	(where_clause)?
;






// call -----------  http://dev.mysql.com/doc/refman/5.6/en/call.html  -------------------------
call_statement:
	CALL_SYM procedure_name (LPAREN ( bit_expr (COMMA bit_expr)* )? RPAREN)?
;






// do --------------  http://dev.mysql.com/doc/refman/5.6/en/do.html  ----------------------------
do_statement:
	DO_SYM root_statement (COMMA root_statement)*
;






// handler ------------  http://dev.mysql.com/doc/refman/5.6/en/handler.html  ----------------------
handler_statements:
	HANDLER_SYM table_name
	(open_handler_statement | handler_statement1 | handler_statement2 | handler_statement3 | close_handler_statement)
;

open_handler_statement:
	OPEN_SYM (alias)?
;

handler_statement1:
	READ_SYM index_name relational_op LPAREN bit_expr (COMMA bit_expr)* RPAREN
	(where_clause)? (limit_clause)?
;

handler_statement2:
	READ_SYM index_name (FIRST_SYM | NEXT_SYM | PREV_SYM | LAST_SYM)
	(where_clause)? (limit_clause)?
;

handler_statement3:
	READ_SYM (FIRST_SYM | NEXT_SYM)
	(where_clause)? (limit_clause)?
;

close_handler_statement:
	CLOSE_SYM
;







// load data ------------  http://dev.mysql.com/doc/refman/5.6/en/load-data.html  ---------------------
load_data_statement:
	LOAD DATA_SYM (LOW_PRIORITY | CONCURRENT)? (LOCAL_SYM)? INFILE TEXT_STRING
	(REPLACE | IGNORE_SYM)?
	INTO TABLE table_spec
	(partition_clause)?
	(CHARACTER_SYM SET_SYM charset_name)?
	(
		(FIELDS_SYM | COLUMNS_SYM)
		(TERMINATED BY_SYM TEXT_STRING)?
		((OPTIONALLY)? ENCLOSED BY_SYM TEXT_STRING)?
		(ESCAPED BY_SYM TEXT_STRING)?
	)?
	(
		LINES
		(STARTING BY_SYM TEXT_STRING)?
		(TERMINATED BY_SYM TEXT_STRING)?
	)?
	(IGNORE_SYM INTEGER_NUM (LINES | ROWS_SYM))?
	(LPAREN (column_spec|USER_VAR) (COMMA (column_spec|USER_VAR))* RPAREN)?
	(set_columns_cluase)?
;






// load xml ---------------  http://dev.mysql.com/doc/refman/5.6/en/load-xml.html  ----------------------
load_xml_statement:
	LOAD XML_SYM (LOW_PRIORITY | CONCURRENT)? (LOCAL_SYM)? INFILE TEXT_STRING
	(REPLACE | IGNORE_SYM)?
	INTO TABLE table_spec
	(partition_clause)?
	(CHARACTER_SYM SET_SYM charset_name)?
	(ROWS_SYM IDENTIFIED_SYM BY_SYM TEXT_STRING)?
	(IGNORE_SYM INTEGER_NUM (LINES | ROWS_SYM))?
	(LPAREN (column_spec|USER_VAR) (COMMA (column_spec|USER_VAR))* RPAREN)?
	(set_columns_cluase)?
;






// replace -------------------  http://dev.mysql.com/doc/refman/5.6/en/replace.html  ---------------------
replace_statement:
	replace_statement_header
	( replace_statement1 | replace_statement2 | replace_statement3 )
;

replace_statement_header:
	REPLACE (LOW_PRIORITY | DELAYED_SYM)?
	(INTO)? table_name
	(partition_clause)?
;

replace_statement1:
	(column_list)?
	value_list_clause
;

replace_statement2:
	set_columns_cluase
;

replace_statement3:
	(column_list)?
	select_statement
;







// http://dev.mysql.com/doc/refman/5.6/en/create-database.html
create_database_statement:
	CREATE (DATABASE | SCHEMA) (IF NOT_SYM EXISTS)? schema_name
	( create_specification (COMMA create_specification)* )*
;
create_specification:
	(DEFAULT)?
	(
		(  CHARACTER_SYM SET_SYM (EQ_SYM)? charset_name  )
		|
		(  COLLATE_SYM (EQ_SYM)? collation_name  )
	)
;


// http://dev.mysql.com/doc/refman/5.6/en/alter-database.html
alter_database_statements:
	alter_database_statement1 | alter_database_statement2
;
alter_database_statement1:
	ALTER (DATABASE | SCHEMA) (schema_name)?
	alter_database_specification
;
alter_database_statement2:
	ALTER (DATABASE | SCHEMA) schema_name
	UPGRADE_SYM DATA_SYM DIRECTORY_SYM NAME_SYM
;
alter_database_specification:
	(DEFAULT)? CHARACTER_SYM SET_SYM (EQ_SYM)? charset_name
	|
	(DEFAULT)? COLLATE_SYM (EQ_SYM)? collation_names

;


// http://dev.mysql.com/doc/refman/5.6/en/drop-database.html
drop_database_statement:
	DROP (DATABASE | SCHEMA) (IF EXISTS)? schema_name
;






// http://dev.mysql.com/doc/refman/5.6/en/create-event.html
create_event_statement:
	CREATE
	(DEFINER EQ_SYM ( user_name | CURRENT_USER ))?
	EVENT_SYM
	(IF NOT_SYM EXISTS)?
	event_name
	ON SCHEDULE_SYM schedule_definition
	(ON COMPLETION_SYM (NOT_SYM)? PRESERVE_SYM)?
	( ENABLE_SYM | DISABLE_SYM | (DISABLE_SYM ON SLAVE) )?
	(COMMENT_SYM TEXT_STRING)?
	do_statement
;
schedule_definition:
	( AT_SYM timestamp (PLUS INTERVAL_SYM interval)* )
	|
	( EVERY_SYM interval )
	( STARTS_SYM timestamp (PLUS INTERVAL_SYM interval)* )?
	( ENDS_SYM timestamp (PLUS INTERVAL_SYM interval)* )?
;
interval:
	INTEGER_NUM (YEAR | QUARTER | MONTH | DAY_SYM | HOUR | MINUTE |
	          WEEK | SECOND | YEAR_MONTH | DAY_HOUR | DAY_MINUTE |
	          DAY_SECOND | HOUR_MINUTE | HOUR_SECOND | MINUTE_SECOND)

;
timestamp:
	CURRENT_TIMESTAMP
	//| timestamp_literal
	//...
;


// http://dev.mysql.com/doc/refman/5.6/en/alter-event.html
alter_event_statement:
	ALTER
	(DEFINER EQ_SYM ( user_name | CURRENT_USER ))?
	EVENT_SYM event_name
	(ON SCHEDULE_SYM schedule_definition)?
	(ON COMPLETION_SYM (NOT_SYM)? PRESERVE_SYM)?
	(RENAME TO_SYM event_name)?
	( ENABLE_SYM | DISABLE_SYM | (DISABLE_SYM ON SLAVE) )?
	(COMMENT_SYM TEXT_STRING)?
	(do_statement)?
;


// http://dev.mysql.com/doc/refman/5.6/en/drop-event.html
drop_event_statement:
	DROP EVENT_SYM (IF EXISTS)? event_name
;





/*
// http://dev.mysql.com/doc/refman/5.6/en/create-function.html
create_function_statement:
;


// http://dev.mysql.com/doc/refman/5.6/en/alter-function.html
alter_function_statement:
	ALTER FUNCTION_SYM function_name (characteristic)*
;
characteristic:
	  ( COMMENT_SYM TEXT_STRING )
	| ( LANGUAGE SQL_SYM )
	| ( (CONTAINS_SYM SQL_SYM) | (NO_SYM SQL_SYM) | (READS_SYM SQL_SYM DATA_SYM) | (MODIFIES_SYM SQL_SYM DATA_SYM) )
	| ( SQL_SYM SECURITY_SYM (DEFINER | INVOKER_SYM) )
;


// http://dev.mysql.com/doc/refman/5.6/en/drop-function.html
drop_function_statement:
;
*/






// http://dev.mysql.com/doc/refman/5.6/en/create-index.html
create_index_statement:
	CREATE (UNIQUE_SYM|FULLTEXT_SYM|SPATIAL_SYM)? INDEX_SYM index_name
	(index_type)?
	ON table_name LPAREN index_column_name (COMMA index_column_name)* RPAREN
	(algorithm_option | lock_option)*
;
algorithm_option:
	ALGORITHM_SYM (EQ_SYM)? (DEFAULT|INPLACE_SYM|COPY_SYM)
;
lock_option:
	LOCK (EQ_SYM)? (DEFAULT|NONE_SYM|SHARED_SYM|EXCLUSIVE_SYM)
;


// http://dev.mysql.com/doc/refman/5.6/en/drop-index.html
drop_index_statement:
	DROP INDEX_SYM index_name ON table_name
	(algorithm_option | lock_option)*
;







/*
// http://dev.mysql.com/doc/refman/5.6/en/create-procedure.html
create_procedure_create_function_statement:
;


// http://dev.mysql.com/doc/refman/5.6/en/alter-procedure.html
alter_procedure_statement:
	ALTER PROCEDURE procedure_name (characteristic)*
;


// http://dev.mysql.com/doc/refman/5.6/en/drop-procedure.html
drop_procedure_drop_function_statement:
;
*/






// http://dev.mysql.com/doc/refman/5.6/en/create-server.html
create_server_statement:
	CREATE SERVER_SYM server_name
	FOREIGN DATA_SYM WRAPPER_SYM wrapper_name
	OPTIONS_SYM LPAREN create_server_option (COMMA create_server_option)* RPAREN
;
create_server_option:
	| ( HOST_SYM string_literal )
	| ( DATABASE string_literal )
	| ( USER string_literal )
	| ( PASSWORD string_literal )
	| ( SOCKET_SYM string_literal )
	| ( OWNER_SYM string_literal )
	| ( PORT_SYM number_literal )
;


// http://dev.mysql.com/doc/refman/5.6/en/alter-server.html
alter_server_statement:
	ALTER SERVER_SYM server_name
	OPTIONS_SYM LPAREN alter_server_option (COMMA alter_server_option)* RPAREN
;
alter_server_option:
	(USER) (ID|TEXT_STRING)
;


// http://dev.mysql.com/doc/refman/5.6/en/drop-server.html
drop_server_statement:
	DROP SERVER_SYM (IF EXISTS)? server_name
;






// http://dev.mysql.com/doc/refman/5.6/en/create-table.html
create_table_statement:
	create_table_statement1 | create_table_statement2 | create_table_statement3
;

create_table_statement1:
	CREATE (TEMPORARY)? TABLE (IF NOT_SYM EXISTS)? table_name
	LPAREN create_definition (COMMA create_definition)* RPAREN
	(table_options)?
	(partition_options)?
	(select_statement)?
;

create_table_statement2:
	CREATE (TEMPORARY)? TABLE (IF NOT_SYM EXISTS)? table_name
	(table_options)?
	(partition_options)?
	select_statement
;

create_table_statement3:
	CREATE (TEMPORARY)? TABLE (IF NOT_SYM EXISTS)? table_name
	( (LIKE_SYM table_name) | (LPAREN LIKE_SYM table_name RPAREN) )
;

create_definition:
	  (  column_name column_definition  )
	| (  (CONSTRAINT (constraint_symbol_name)?)? PRIMARY_SYM KEY_SYM (index_type)? LPAREN index_column_name (COMMA index_column_name)* RPAREN (index_option)*  )
	| (  (INDEX_SYM|KEY_SYM) (index_name)? (index_type)? LPAREN index_column_name (COMMA index_column_name)* RPAREN (index_option)*  )
	| (  (CONSTRAINT (constraint_symbol_name)?)? UNIQUE_SYM (INDEX_SYM|KEY_SYM)? (index_name)? (index_type)? LPAREN index_column_name (COMMA index_column_name)* RPAREN (index_option)*  )
	| (  (FULLTEXT_SYM|SPATIAL_SYM) (INDEX_SYM|KEY_SYM)? (index_name)? LPAREN index_column_name (COMMA index_column_name)* RPAREN (index_option)*  )
	| (  (CONSTRAINT (constraint_symbol_name)?)? FOREIGN KEY_SYM (index_name)? LPAREN index_column_name (COMMA index_column_name)* RPAREN reference_definition  )
	| (  CHECK_SYM LPAREN expression RPAREN  )
;

column_definition:
	column_data_type_header
	(AUTO_INCREMENT)? ( (UNIQUE_SYM (KEY_SYM)?) | (PRIMARY_SYM (KEY_SYM)?) )?
	(COMMENT_SYM TEXT_STRING)?
	(COLUMN_FORMAT (FIXED_SYM|DYNAMIC_SYM|DEFAULT))?
	(reference_definition)?
;

null_or_notnull:
	(NOT_SYM NULL_SYM) | NULL_SYM
;

column_data_type_header:
	  (  BIT_SYM(LPAREN length RPAREN)? (null_or_notnull)? (DEFAULT bit_literal)?  )
	| (  TINYINT(LPAREN length RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  SMALLINT(LPAREN length RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  MEDIUMINT(LPAREN length RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  INT_SYM(LPAREN length RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  INTEGER_SYM(LPAREN length RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  BIGINT(LPAREN length RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  REAL(LPAREN length COMMA number_literal RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  DOUBLE_SYM(LPAREN length COMMA number_literal RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  FLOAT_SYM(LPAREN length COMMA number_literal RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  DECIMAL_SYM(LPAREN length( COMMA number_literal)? RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  NUMERIC_SYM(LPAREN length( COMMA number_literal)? RPAREN)? (UNSIGNED_SYM)? (ZEROFILL)? (null_or_notnull)? (DEFAULT number_literal)?  )
	| (  DATE_SYM (null_or_notnull)? (DEFAULT TEXT_STRING)?  )
	| (  TIME_SYM (null_or_notnull)? (DEFAULT TEXT_STRING)?  )
	| (  TIMESTAMP (null_or_notnull)? (DEFAULT (CURRENT_TIMESTAMP|TEXT_STRING))?  )
	| (  DATETIME (null_or_notnull)? (DEFAULT (CURRENT_TIMESTAMP|TEXT_STRING))?  )
	| (  YEAR (null_or_notnull)? (DEFAULT INTEGER_NUM)?  )
	| (  CHAR   (LPAREN length RPAREN)? (CHARACTER_SYM SET_SYM charset_name)? (COLLATE_SYM collation_name)? (null_or_notnull)? (DEFAULT TEXT_STRING)?  )
	| (  VARCHAR LPAREN length RPAREN   (CHARACTER_SYM SET_SYM charset_name)? (COLLATE_SYM collation_name)? (null_or_notnull)? (DEFAULT TEXT_STRING)?  )
	| (  BINARY   (LPAREN length RPAREN)? (null_or_notnull)? (DEFAULT TEXT_STRING)?  )
	| (  VARBINARY LPAREN length RPAREN (null_or_notnull)? (DEFAULT TEXT_STRING)?  )
	| (  TINYBLOB (null_or_notnull)?  )
	| (  BLOB_SYM (null_or_notnull)?  )
	| (  MEDIUMBLOB (null_or_notnull)?  )
	| (  LONGBLOB (null_or_notnull)?  )
	| (  TINYTEXT   (BINARY)? (CHARACTER_SYM SET_SYM charset_name)? (COLLATE_SYM collation_name)? (null_or_notnull)?  )
	| (  TEXT_SYM   (BINARY)? (CHARACTER_SYM SET_SYM charset_name)? (COLLATE_SYM collation_name)? (null_or_notnull)?  )
	| (  MEDIUMTEXT (BINARY)? (CHARACTER_SYM SET_SYM charset_name)? (COLLATE_SYM collation_name)? (null_or_notnull)?  )
	| (  LONGTEXT   (BINARY)? (CHARACTER_SYM SET_SYM charset_name)? (COLLATE_SYM collation_name)? (null_or_notnull)?  )
	| (  ENUM    LPAREN TEXT_STRING (COMMA TEXT_STRING)* RPAREN (CHARACTER_SYM SET_SYM charset_name)? (COLLATE_SYM collation_name)? (null_or_notnull)? (DEFAULT TEXT_STRING)?  )
	| (  SET_SYM LPAREN TEXT_STRING (COMMA TEXT_STRING)* RPAREN (CHARACTER_SYM SET_SYM charset_name)? (COLLATE_SYM collation_name)? (null_or_notnull)? (DEFAULT TEXT_STRING)?  )
	//| (  spatial_type (null_or_notnull)? (DEFAULT default_value)?  )
;

index_column_name:
	column_name (LPAREN INTEGER_NUM RPAREN)? (ASC | DESC)?
;

reference_definition:
	REFERENCES table_name LPAREN index_column_name (COMMA index_column_name)* RPAREN
	( (MATCH FULL) | (MATCH PARTIAL) | (MATCH SIMPLE_SYM) )?
	(ON DELETE_SYM reference_option)?
	(ON UPDATE reference_option)?
;
reference_option:
	(RESTRICT) | (CASCADE) | (SET_SYM NULL_SYM) | (NO_SYM ACTION)
;

table_options:
	table_option (( COMMA )? table_option)*
;
table_option:
	  (  ENGINE_SYM (EQ_SYM)? engine_name  )
	| (  AUTO_INCREMENT (EQ_SYM)? INTEGER_NUM  )
	| (  AVG_ROW_LENGTH (EQ_SYM)? INTEGER_NUM  )
	| (  (DEFAULT)? CHARACTER_SYM SET_SYM (EQ_SYM)? charset_name  )
	| (  CHECKSUM_SYM (EQ_SYM)? INTEGER_NUM  )
	| (  (DEFAULT)? COLLATE_SYM (EQ_SYM)? collation_name  )
	| (  COMMENT_SYM (EQ_SYM)? TEXT_STRING  )
	| (  CONNECTION_SYM (EQ_SYM)? TEXT_STRING  )
	| (  DATA_SYM DIRECTORY_SYM (EQ_SYM)? TEXT_STRING  )
	| (  DELAY_KEY_WRITE_SYM (EQ_SYM)? INTEGER_NUM  )
	| (  INDEX_SYM DIRECTORY_SYM (EQ_SYM)? TEXT_STRING  )
	| (  INSERT_METHOD (EQ_SYM)? ( NO_SYM | FIRST_SYM | LAST_SYM )  )
	| (  KEY_BLOCK_SIZE (EQ_SYM)? INTEGER_NUM  )
	| (  MAX_ROWS (EQ_SYM)? INTEGER_NUM  )
	| (  MIN_ROWS (EQ_SYM)? INTEGER_NUM  )
	| (  PACK_KEYS_SYM (EQ_SYM)? (INTEGER_NUM | DEFAULT)  )
	| (  PASSWORD (EQ_SYM)? TEXT_STRING  )
	| (  ROW_FORMAT_SYM (EQ_SYM)? (DEFAULT|DYNAMIC_SYM|FIXED_SYM|COMPRESSED_SYM|REDUNDANT_SYM|COMPACT_SYM)  )
	| (  STATS_AUTO_RECALC (EQ_SYM)? (DEFAULT | INTEGER_NUM)  )
	| (  STATS_PERSISTENT (EQ_SYM)? (DEFAULT | INTEGER_NUM)  )
	| (  UNION_SYM (EQ_SYM)? LPAREN table_name( COMMA table_name)* RPAREN  )
;

partition_options:
	PARTITION_SYM BY_SYM
	(
		  ( (LINEAR_SYM)? HASH_SYM LPAREN expression RPAREN )
		| ( (LINEAR_SYM)? KEY_SYM LPAREN column_list RPAREN )
		| ( RANGE_SYM(LPAREN expression RPAREN | COLUMNS_SYM LPAREN column_list RPAREN) )
		| ( LIST_SYM(LPAREN expression RPAREN | COLUMNS_SYM LPAREN column_list RPAREN) )
	)

	(PARTITIONS_SYM INTEGER_NUM)?

	(
		SUBPARTITION_SYM BY_SYM
		( ( (LINEAR_SYM)? HASH_SYM LPAREN expression RPAREN ) | ( (LINEAR_SYM)? KEY_SYM LPAREN column_list RPAREN ) )
		(SUBPARTITIONS_SYM INTEGER_NUM)?
	)?

	(LPAREN partition_definition ( COMMA  partition_definition)* RPAREN)?
;

partition_definition:
	PARTITION_SYM partition_name

	(
		VALUES
		(
			(LESS_SYM THAN_SYM ( (LPAREN expression_list RPAREN) | MAXVALUE_SYM ))
			|
			(IN_SYM LPAREN expression_list RPAREN)
		)
	)?

	((STORAGE_SYM)? ENGINE_SYM (EQ_SYM)? engine_name)?
	(COMMENT_SYM (EQ_SYM)? TEXT_STRING )?
	(DATA_SYM DIRECTORY_SYM (EQ_SYM)? TEXT_STRING)?
	(INDEX_SYM DIRECTORY_SYM (EQ_SYM)? TEXT_STRING)?
	(MAX_ROWS (EQ_SYM)? INTEGER_NUM)?
	(MIN_ROWS (EQ_SYM)? INTEGER_NUM)?
	(LPAREN subpartition_definition (COMMA  subpartition_definition)* RPAREN)?
;

subpartition_definition:
	SUBPARTITION_SYM partition_logical_name
	((STORAGE_SYM)? ENGINE_SYM (EQ_SYM)? engine_name)?
	(COMMENT_SYM (EQ_SYM)? TEXT_STRING )?
	(DATA_SYM DIRECTORY_SYM (EQ_SYM)? TEXT_STRING)?
	(INDEX_SYM DIRECTORY_SYM (EQ_SYM)? TEXT_STRING)?
	(MAX_ROWS (EQ_SYM)? INTEGER_NUM)?
	(MIN_ROWS (EQ_SYM)? INTEGER_NUM)?
;

length	:	INTEGER_NUM;


// http://dev.mysql.com/doc/refman/5.6/en/alter-table.html
alter_table_statement:
	ALTER (IGNORE_SYM)? TABLE table_name
	( alter_table_specification (COMMA alter_table_specification)* )?
	( partition_options )?
;
alter_table_specification:
	  table_options
	| ( ADD_SYM (COLUMN_SYM)? column_name column_definition ( (FIRST_SYM|AFTER_SYM) column_name )? )
	| ( ADD_SYM (COLUMN_SYM)? LPAREN column_definitions RPAREN )
	| ( ADD_SYM (INDEX_SYM|KEY_SYM) (index_name)? (index_type)? LPAREN index_column_names RPAREN (index_option)* )
	| ( ADD_SYM (CONSTRAINT (constraint_symbol_name)?)? PRIMARY_SYM KEY_SYM (index_type)? LPAREN index_column_names RPAREN (index_option)* )
	|
		(
		ADD_SYM (CONSTRAINT (constraint_symbol_name)?)? UNIQUE_SYM (INDEX_SYM|KEY_SYM)? (index_name)?
		(index_type)? LPAREN index_column_name (COMMA index_column_name)* RPAREN (index_option)*
		)
	| ( ADD_SYM FULLTEXT_SYM (INDEX_SYM|KEY_SYM)? (index_name)? LPAREN index_column_names RPAREN (index_option)* )
	| ( ADD_SYM SPATIAL_SYM (INDEX_SYM|KEY_SYM)? (index_name)? LPAREN index_column_names RPAREN (index_option)* )
	| ( ADD_SYM (CONSTRAINT (constraint_symbol_name)?)? FOREIGN KEY_SYM (index_name)? LPAREN index_column_names RPAREN reference_definition )
	| ( ALGORITHM_SYM (EQ_SYM)? (DEFAULT|INPLACE_SYM|COPY_SYM) )
	| ( ALTER (COLUMN_SYM)? column_name ((SET_SYM DEFAULT literal_value) | (DROP DEFAULT)) )
	| ( CHANGE (COLUMN_SYM)? column_name column_name column_definition (FIRST_SYM|AFTER_SYM column_name)? )
	| ( LOCK (EQ_SYM)? (DEFAULT|NONE_SYM|SHARED_SYM|EXCLUSIVE_SYM) )
	| ( MODIFY_SYM (COLUMN_SYM)? column_name column_definition (FIRST_SYM | AFTER_SYM column_name)? )
	| ( DROP (COLUMN_SYM)? column_name )
	| ( DROP PRIMARY_SYM KEY_SYM )
	| ( DROP (INDEX_SYM|KEY_SYM) index_name )
	| ( DROP FOREIGN KEY_SYM foreign_key_symbol_name )
	| ( DISABLE_SYM KEYS )
	| ( ENABLE_SYM KEYS )
	| ( RENAME (TO_SYM|AS_SYM)? table_name )
	| ( ORDER_SYM BY_SYM column_name (COMMA column_name)* )
	| ( CONVERT_SYM TO_SYM CHARACTER_SYM SET_SYM charset_name (COLLATE_SYM collation_name)? )
	| ( (DEFAULT)? CHARACTER_SYM SET_SYM (EQ_SYM)? charset_name (COLLATE_SYM (EQ_SYM)? collation_name)? )
	| ( DISCARD TABLESPACE )
	| ( IMPORT TABLESPACE )
	| ( FORCE_SYM )
	| ( ADD_SYM PARTITION_SYM LPAREN partition_definition RPAREN )
	| ( DROP PARTITION_SYM partition_names )
	| ( TRUNCATE PARTITION_SYM (partition_names | ALL) )
	| ( COALESCE PARTITION_SYM INTEGER_NUM )
	| ( REORGANIZE_SYM PARTITION_SYM partition_names INTO LPAREN partition_definition (COMMA partition_definition)* RPAREN )
	| ( EXCHANGE_SYM PARTITION_SYM partition_name WITH TABLE table_name )
	| ( ANALYZE_SYM PARTITION_SYM (partition_names | ALL) )
	| ( CHECK_SYM PARTITION_SYM (partition_names | ALL) )
	| ( OPTIMIZE PARTITION_SYM (partition_names | ALL) )
	| ( REBUILD_SYM PARTITION_SYM (partition_names | ALL) )
	| ( REPAIR PARTITION_SYM (partition_names | ALL) )
	| ( REMOVE_SYM PARTITIONING_SYM )
;
index_column_names:
	index_column_name (COMMA index_column_name)*;
index_type:
	USING_SYM (BTREE_SYM | HASH_SYM)
;
index_option:
	  ( KEY_BLOCK_SIZE (EQ_SYM)? INTEGER_NUM )
	| index_type
	| ( WITH PARSER_SYM parser_name )
	| ( COMMENT_SYM TEXT_STRING )
;
column_definitions:
	column_name column_definition (COMMA column_name column_definition)*
;


// http://dev.mysql.com/doc/refman/5.6/en/rename-table.html
rename_table_statement:
	RENAME TABLE
	table_name TO_SYM table_name
	(COMMA table_name TO_SYM table_name)*
;


// http://dev.mysql.com/doc/refman/5.6/en/drop-table.html
drop_table_statement:
	DROP (TEMPORARY)? TABLE (IF EXISTS)?
	table_name (COMMA table_name)*
	(RESTRICT | CASCADE)?
;


// http://dev.mysql.com/doc/refman/5.6/en/truncate-table.html
truncate_table_statement:
	TRUNCATE (TABLE)? table_name
;

/*
// http://dev.mysql.com/doc/refman/5.6/en/create-trigger.html
create_trigger_statement:
	CREATE
	(DEFINER EQ_SYM (user_name | CURRENT_USER))?
	// ...
;


// http://dev.mysql.com/doc/refman/5.6/en/drop-trigger.html
drop_trigger_statement:
;
*/

// http://dev.mysql.com/doc/refman/5.6/en/create-view.html
create_view_statement:
	CREATE (OR_SYM REPLACE)?
	create_view_body
;
create_view_body:
	(ALGORITHM_SYM EQ_SYM (UNDEFINED_SYM | MERGE_SYM | TEMPTABLE_SYM))?
	(DEFINER EQ_SYM (user_name | CURRENT_USER) )?
	(SQL_SYM SECURITY_SYM ( DEFINER | INVOKER_SYM ))?
	VIEW_SYM view_name (LPAREN column_list RPAREN)?
	AS_SYM select_statement
	(WITH (CASCADED | LOCAL_SYM)? CHECK_SYM OPTION)?
;


// http://dev.mysql.com/doc/refman/5.6/en/alter-view.html
alter_view_statement:
	ALTER
	create_view_body
;


// http://dev.mysql.com/doc/refman/5.6/en/drop-view.html
drop_view_statement:
	DROP VIEW_SYM (IF EXISTS)?
	view_name (COMMA view_name)*
	(RESTRICT | CASCADE)?
;

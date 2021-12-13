/*
 * Copyright (c) 2021 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *   Broadcom, Inc. - initial API and implementation
 */

parser grammar IdmsParser;
options {tokenVocab = CobolLexer; superClass = MessageServiceParser;}

// -- schema section ----------------------------------

schemaSection
   : SCHEMA SECTION DOT_FS schemaDBEntry
   ;

schemaDBEntry
   : DB idms_subschema_name WITHIN idms_schema_name versionClause? DOT_FS
   ;

// -- map section ----------------------------------

mapSection
   : MAP SECTION DOT_FS maxFieldListClause? mapClause+
   ;

maxFieldListClause
   :  MAX FIELD LIST IS? integerLiteral DOT_FS?
   ;

mapClause
    : MAP idms_map_name_definition versionClause? (TYPE IS? (STANDARD | EXTENDED) PAGING?)? DOT_FS?
    ;

versionClause
    : VERSION integerLiteral
    ;

// -- idms control section ----------------------------------

idmsControlSection
   : IDMS_CONTROL SECTION DOT_FS idmsControlSectionParagraph
   ;

// - idms control section paragraph ----------------------------------
idmsControlSectionParagraph
   : protocolParagraph (protocolParagraphs COMMACHAR?)*
   ;

protocolParagraphs
   : ssNamesLengthParagraph | idmsRecordLocationParagraph
   ;

protocolParagraph
   : PROTOCOL DOT_FS? protocolEntry?
   ;

protocolEntry
   : modeClause DEBUG? endClause?
   ;

modeClause
   : MODE IS? dataName
   ;

ssNamesLengthParagraph
   : SUBSCHEMA_NAMES LENGTH IS? ss_names_length endClause?
   ;

idmsRecordLocationParagraph
   : IDMS_RECORDS withinClause endClause?
   ;

withinClause
   : (withinEntry | MANUAL) levelsClause?
   ;

withinEntry
   : WITHIN (WORKING_STORAGE | LINKAGE) SECTION?
   ;

levelsClause
   : LEVELS? INCREMENTED BY? LEVEL_NUMBER
   ;

ss_names_length
    : {validateSubSchemaNameLength(_input.LT(1).getText());} LEVEL_NUMBER
    ;

// statements

idmsStatements
    : idmsStmtsOptTermOn endClause? idmsOnClause? | idmsStmtsMandTermOn (SEMICOLON_FS idmsOnClause? | DOT_FS)
    ;

idmsStmtsOptTermOn
    : abendCodeStatement | attachTaskCodeStatement | bindStatement | changePriorityStatement | checkTerminalStatement
    | commitStatement | connectStatement | dcStatement | dequeueStatement | disconnectStatement | endStatement
    | endpageStatement | enqueueStatement | eraseStatement | findStatement | finishStatement | freeStatement
    | getStatement | idmsAcceptStatement | idmsIfStatement | idmsReadStatement | idmsReturnStatement | idmsSendStatement
    | idmsSetStatement | inquireMapStatement | keepStatement | loadStatement | mapStatement | modifyStatement
    | obtainStatement | postStatement | putStatement | readyStatement | rollbackStatement
    | snapStatement | startpageStatement | storeStatement | waitStatement
    ;

idmsStmtsMandTermOn
    : transferStatement
    ;

idmsOnClause
    : ON generalIdentifier
    ;

// abend code statement

abendCodeStatement
    : ABEND CODE (literal | generalIdentifier) abendCodeDumpClause? abendCodeExitClause?
    ;

abendCodeDumpClause
    : (DUMP | NODUMP)
    ;

abendCodeExitClause
    : EXITS (INVOKED | IGNORED)
    ;

// accept statement

idmsAcceptStatement
   : ACCEPT (acceptIdmsDcClause | acceptIdmsDbClause) idmsOnClause?
   ;

acceptIdmsDcClause
   : acceptTransactionStatisticsClause | ((LTERM ID | PTERM ID | SCREENSIZE | SYSTEM ID | SYSVERSION | TASK CODE | TASK ID | USER ID) INTO generalIdentifier)
   ;

acceptTransactionStatisticsClause
    : TRANSACTION STATISTICS acceptTransactionStatisticsWriteClause? acceptTransactionStatisticsIntoClause? acceptTransactionStatisticsLengthClause?
    ;

acceptTransactionStatisticsWriteClause
    : (WRITE | NOWRITE)
    ;

acceptTransactionStatisticsIntoClause
    : INTO generalIdentifier
    ;

acceptTransactionStatisticsLengthClause
    : LENGTH (integerLiteral | generalIdentifier)
    ;

acceptIdmsDbClause
    : generalIdentifier ((FROM acceptIdmsDbOptions) | FOR idms_db_entity_name)
    ;

acceptIdmsDbOptions
    : (idms_procedure_name PROCEDURE) | currencyPageInfo | (idms_db_entity_name acceptIdmsTypes) |
     (IDMS_STATISTICS (EXTENDED generalIdentifier)?)
    ;

acceptIdmsTypes
    : (BIND | ((NEXT | PRIOR |OWNER)? currencyPageInfo))
    ;

currencyPageInfo
    : CURRENCY (PAGE_INFO generalIdentifier)?
    ;

// accept transaction statistics statement

attachTaskCodeStatement
    : ATTACH TASK CODE (generalIdentifier | literal) attachTaskCodePriorityClause? idmsWaitNowaitClause?
    ;

attachTaskCodePriorityClause
    : PRIORITY (integerLiteral | generalIdentifier)
    ;

// bind statement

bindStatement
    : BIND (bindTaskClause | bindTransactionClause | bindRunUnitClause | bindMapClause | bindProcedureClause |bindRecordClause)
    ;

bindMapClause
    : MAP idms_map_name (RECORD idms_db_entity_name (TO (NULL | generalIdentifier))?)?
    ;

bindProcedureClause
    : PROCEDURE FOR idms_procedure_name TO generalIdentifier
    ;

bindTaskClause
    : TASK bindTaskStatementNodenameClause?
    ;

bindTaskStatementNodenameClause
    : NODENAME (generalIdentifier | literal)
    ;

bindTransactionClause
    : TRANSACTION STATISTICS
    ;

bindRunUnitClause
    : RUN_UNIT (FOR generalIdentifier)? (DBNODE bindDbNodeName)? (DBNAME bindDbNodeName)? (DICTNODE bindDbNodeName)? (DICTNAME bindDbNodeName)?
    ;

bindRecordClause
    : (idms_db_entity_name (TO generalIdentifier)?) | (generalIdentifier WITH idms_db_entity_name)
    ;

bindDbNodeName
    : literal | generalIdentifier
    ;

// change priority statement

changePriorityStatement
    : CHANGE PRIORITY TO? (integerLiteral | generalIdentifier)
    ;

// check terminal statement

checkTerminalStatement
    : CHECK TERMINAL checkTerminalGetStorageClause? INTO generalIdentifier (checkTerminalIntoClause | checkTerminalMaxLengthClause) checkTerminalReturnLengthClause?
    ;

checkTerminalGetStorageClause
    : GET STORAGE
    ;

checkTerminalIntoClause
    : TO generalIdentifier
    ;

checkTerminalMaxLengthClause
    : MAX LENGTH (generalIdentifier | integerLiteral)
    ;

checkTerminalReturnLengthClause
    : RETURN LENGTH INTO? generalIdentifier
    ;

// commit statement

commitStatement
   : COMMIT TASK? ALL?
   ;

// connect statement

connectStatement
   : CONNECT idms_db_entity_name TO idms_db_entity_name
   ;

// dc statement

dcStatement
    : DC RETURN dcNextTaskCodeClause? dcOptionClause? dcTimeoutClause? dcNextTaskIntervalClause?
    ;

dcNextTaskCodeClause
    : NEXT TASK CODE (generalIdentifier | literal)
    ;

dcOptionClause
    : (NORMAL | ABORT | CONTINUE | IMMEDIATE)
    ;

dcTimeoutClause
    : TIMEOUT (dcIntervalClause | dcProgramClause)*
    ;

dcNextTaskIntervalClause
    : NEXT TASK INTERVAL (generalIdentifier | integerLiteral) EVENT TYPE (INTERNAL | EXTERNAL) dcEventClause?
    ;

dcIntervalClause
    : INTERVAL (generalIdentifier | integerLiteral)
    ;

dcProgramClause
    : PROGRAM (generalIdentifier | literal)
    ;

dcEventClause
    : (EVENT generalIdentifier) | (EVENT NAME (generalIdentifier | literal))
    ;

// dequeue statement

dequeueStatement
    : DEQUEUE (ALL | dequeueNameStatement+)
    ;

dequeueNameStatement
    : NAME generalIdentifier LENGTH (generalIdentifier | integerLiteral)
    ;

// disconnect statement

disconnectStatement
   : DISCONNECT idms_db_entity_name FROM idms_db_entity_name
   ;

// end statement

endStatement
   : END (endLineClause | endTransactionClause)
   ;

endLineClause
   : LINE TERMINAL SESSION?
   ;

endTransactionClause
   : TRANSACTION STATISTICS endTransactionWriteClause? endTransactionIntoClause? endTransactionLengthClause?
   ;

endTransactionWriteClause
   : (WRITE | NOWRITE)
   ;

endTransactionIntoClause
   : INTO generalIdentifier
   ;

endTransactionLengthClause
   : LENGTH (generalIdentifier | integerLiteral)
   ;

// endpage statement

endpageStatement
   : ENDPAGE SESSION?
   ;

// enqueue statement

enqueueStatement
   : ENQUEUE (WAIT | NOWAIT | TEST)? enqueueNameClause*
   ;

enqueueNameClause
   : NAME generalIdentifier LENGTH (generalIdentifier | integerLiteral) (EXCLUSIVE | SHARED)?
   ;

// erase statement

eraseStatement
   : ERASE idms_db_entity_name ((PERMANENT | SELECTIVE | ALL) MEMBERS)?
   ;

// find statement

findStatement
   : FIND keepClause? findObtainClause
   ;

keepClause
    : KEEP EXCLUSIVE?
    ;

findObtainClause
    : calcClause | currentClause | ownerClause | recnameClause | dbkeyClause | positionClause
    ;

calcClause
    : (CALC | ANY | DUPLICATE) idms_db_entity_name
    ;

currentClause
    : CURRENT idms_db_entity_name? (WITHIN idms_db_entity_name)?
    ;

ownerClause
    : OWNER WITHIN idms_db_entity_name
    ;

recnameClause
    : idms_db_entity_name (DB_KEY IS? generalIdentifier | WITHIN idms_db_entity_name CURRENT? USING generalIdentifier)
    ;

dbkeyClause
    : DB_KEY IS? generalIdentifier (PAGE_INFO generalIdentifier)?
    ;

positionClause
    : (orderClause | integerLiteral | generalIdentifier) idms_db_entity_name? WITHIN idms_db_entity_name
    ;

orderClause
    : ( NEXT | PRIOR | FIRST | LAST )
    ;

// finish statement

finishStatement
   : FINISH TASK?
   ;

// free statement

freeStatement
    : FREE STORAGE (freeStgidClause | freeForClause)
    ;

freeStgidClause
    : STGID (generalIdentifier | literal)
    ;

freeForClause
    : FOR generalIdentifier (FROM generalIdentifier)?
    ;

// get statement
getStatement
    : GET (getTimeClause | idms_db_entity_name | getQueueClause | getScratchClause | getStorageClause)?
    ;

getQueueClause
    : QUEUE (ID (generalIdentifier | literal))? getQueueTypeClause? getStatClause? getQueueLockClause?  idmsWaitNowaitClause? INTO generalIdentifier getLengthClause getReturnClause?
    ;

getQueueTypeClause
    : (NEXT | FIRST | LAST | PRIOR | (SEQUENCE (generalIdentifier | integerLiteral)) | (RECORD ID (generalIdentifier | literal)))
    ;

getStatClause
    : (DELETE | KEEP)
    ;

getQueueLockClause
    : (LOCK | NOLOCK)
    ;

getLengthClause
    : ((TO generalIdentifier) | (MAX LENGTH (generalIdentifier | literal)))
    ;

getReturnClause
    : RETURN LENGTH INTO generalIdentifier
    ;

getScratchClause
    : SCRATCH getScratchAreaClause? getScratchNextClause? getStatClause? INTO generalIdentifier getLengthClause getReturnClause?
    ;

getScratchAreaClause
    : AREA ID (generalIdentifier | literal)?
    ;

getScratchNextClause
    : (NEXT | FIRST | LAST | PRIOR | CURRENT | (RECORD ID generalIdentifier))
    ;

getStorageClause
    : STORAGE FOR generalIdentifier (TO generalIdentifier)? (LENGTH generalIdentifier)? (POINTER generalIdentifier)?
    idmsWaitNowaitClause? KEEP? (LONG | SHORT)? (USER | SHARED)? (STGID (generalIdentifier | literal))?
    getStorageValueClause? getStorageLocClause?
    ;

getStorageValueClause
    : VALUE IS (LOW_VALUE | HIGH_VALUE | generalIdentifier)
    ;

getStorageLocClause
    : LOCATION IS? (ANY | BELOW)?
    ;

getTimeClause
    : TIME getTimeIntoClause? (DATE INTO generalIdentifier)?
    ;

getTimeIntoClause
    : INTO generalIdentifier (COMP | COMP_3 | EDIT)
    ;

// if statement

idmsIfStatement
   : IF idmsIfCondition idmsIfThen idmsIfElse? END_IF?
   ;

idmsIfThen
   : THEN? (NEXT SENTENCE | idmsStatements+)
   ;

idmsIfElse
   : ELSE (NEXT SENTENCE | idmsStatements+)
   ;

idmsIfCondition
   : (idms_db_entity_name idmsIfEmpty) | (idmsIfMember)
   ;

idmsIfEmpty
    : IS? NOT? EMPTY
    ;

idmsIfMember
    : NOT? idms_db_entity_name MEMBER
    ;

// inquire map statement

inquireMapStatement
   : INQUIRE MAP idms_map_name (MOVE inqMapMovePhrase | IF inqMapIfPhrase)
   ;

inqMapMovePhrase
   : (AID TO generalIdentifier) | (CURSOR TO generalIdentifier generalIdentifier) | (IN LENGTH FOR generalIdentifier TO generalIdentifier)
   ;

inqMapIfPhrase
   : (INPUT (UNFORMATTED | TRUNCATED | CHANGED | EXTRANEOUS) | (CURSOR AT? DFLD generalIdentifier) |
   (inqMapWhichFields | inqMapWhichDflds) inqMapFieldTestPhrase) idmsIfThen idmsIfElse?
   ;

inqMapWhichFields
   : CURRENT | ALL | NONE | ANY | SOME | ALL (BUT | EXCEPT) CURRENT
   ;

inqMapWhichDflds
   : (ALL | NONE | ANY | SOME | ALL (BUT | EXCEPT))? (DFLD generalIdentifier)+?
   ;

inqMapFieldTestPhrase
   : DATA IS? (YES | NO | ERASE | TRUNCATED | IDENTICAL | DIFFERENT) | mapEditPhrase
   ;

mapEditPhrase
   : EDIT IS? (ERROR | CORRECT)
   ;

// keep statement

keepStatement
    : KEEP (keepCurrentClause | keepLongtermClause)
    ;

keepCurrentClause
    :  EXCLUSIVE? currentClause
    ;

keepLongtermClause
    : LONGTERM (ALL | (generalIdentifier | literal)) keepLongtermRestClause
    ;

keepLongtermRestClause
    : (keepLongtermNotifyClause | keepLongtermLockClause | keepLongtermTestClause | RELEASE)
    ;

keepLongtermNotifyClause
    : NOTIFY CURRENT idms_db_entity_name
    ;

keepLongtermLockClause
    : ((UPGRADE (SHARE | EXCLUSIVE) (RETURN NOTIFICATION INTO? generalIdentifier)) | ((SHARE | EXCLUSIVE) CURRENT idms_db_entity_name)) (WAIT | NOWAIT | NODEADLOCK)?
    ;

keepLongtermTestClause
    : TEST (RETURN NOTIFICATION INTO? generalIdentifier)?
    ;

// load Statement

loadStatement
    : LOAD TABLE (generalIdentifier | idms_table_name) INTO generalIdentifier loadLocationClause idmsDictnodeClause? idmsDictnameClause? loadLoadlibClause? idmsWaitNowaitClause
    ;

loadLocationClause
    : (TO | POINTER) generalIdentifier
    ;

loadLoadlibClause
    : LOADLIB (generalIdentifier | literal)
    ;

// map statement

mapStatement
    : MAP (mapInClause | mapOutClause | mapOutInClause)
    ;

mapInClause
    : IN USING idms_map_name mapIoInputPhrase? mapDetailPhrase?
    ;

mapIoInputPhrase
    : mapInIoPhrase | mapInputPhrase
    ;

mapInIoPhrase
    : (IO mapInputPhrase? | (NOIO DATASTREAM idmsDmlFromClause))
    ;

mapInputPhrase
    : INPUT DATA IS? (YES | NO)
    ;

mapDetailPhrase
    : ((DETAIL mapDetailOptions?) | HEADER ) ((PAGE IS? generalIdentifier) | MODIFIED)*
    ;

mapDetailOptions
    : (NEXT | FIRST | (SEQUENCE NUMBER IS? generalIdentifier) | (KEY IS? generalIdentifier))
    (RETURNKEY IS? generalIdentifier)?
    ;

mapOutClause
    : OUT USING idms_map_name  idmsWaitNowaitClause?  mapOutIoPhrase? mapOutputPhrase? mapMessagePhrase? mapOutDetailPhrase?
    ;

mapOutIoPhrase
    : (IO | (NOIO DATASTREAM mapOutIntoClause))
    ;

mapOutIntoClause
    : INTO? generalIdentifier ((TO generalIdentifier) | (MAX? LENGTH (generalIdentifier | integerLiteral)))
      (RETURN LENGTH INTO? generalIdentifier)?
    ;

mapOutputPhrase
    : OUTPUT ((DATA IS? (YES | NO | ERASE | ATTRIBUTE))? (NEWPAGE | ERASE)? LITERALS?)
    ;

mapMessagePhrase
    : MESSAGE IS? generalIdentifier idmsDmlLengthClause
    ;

mapOutDetailPhrase
    : (DETAIL (NEW | CURRENT)? (KEY IS? generalIdentifier)?) |
      (RESUME (PAGE IS? (CURRENT | NEXT | PRIOR | FIRST | LAST | generalIdentifier))?)
    ;

mapOutInClause
    : OUTIN USING idms_map_name mapOutputPhrase? mapInputPhrase? mapMessagePhrase?
    ;

idmsDictnameClause
    : DICTNAME (generalIdentifier | idms_dictionary_name)
    ;

idmsDictnodeClause
    : DICTNODE (generalIdentifier | idms_node_name)
    ;

idmsDmlFromClause
    : FROM generalIdentifier idmsDmlLengthClause
    ;

idmsDmlLengthClause
   : ((TO generalIdentifier) | (LENGTH (generalIdentifier | integerLiteral)))
   ;

idmsWaitNowaitClause
    : (WAIT | NOWAIT)
    ;

// modify statement
modifyStatement
    : MODIFY  ((MAP modifyMapClause) | idms_db_entity_name )
    ;

// modify map statement
modifyMapClause
    : idms_map_name (PERMANENT | TEMPORARY)?
     (CURSOR AT? ((DFLD generalIdentifier) | (generalIdentifier | integerLiteral) (generalIdentifier | integerLiteral)))?
     (WCC ((RESETMDT | NOMDT) | (RESETKBD | NOKBD) | (ALARM | NOALARM) | (STARTPRT | NOPRT) |
     (NLCR | FORTYCR | SIXTYFOURCR | EIGHTYCR))+)? (modifyMapForClause modifyMapFieldOptionsClause)?
    ;

modifyMapForClause
    : FOR ((ALL ((BUT | EXCEPT) (CURRENT | (DFLD generalIdentifier)+) | (ERROR | CORRECT)? FIELDS)) |
      (ALL? (DFLD generalIdentifier)+))
    ;

modifyMapFieldOptionsClause
    : (BACKSCAN | NOBACKSCAN)? (OUTPUT DATA IS? (YES | NO | ERASE | ATTRIBUTE))? mapInputPhrase?
    ((RIGHT | LEFT)? JUSTIFY)? (PAD (LOW_VALUE | HIGH_VALUE | (literal | generalIdentifier)))?
    mapEditPhrase? (REQUIRED | OPTIONAL)? (ERROR MESSAGE IS? (ACTIVE | SUPPRESS))? (ATTRIBUTES (attributeList)+)?
    ;

attributeList
    : SKIPCHAR | ALPHANUMERIC | NUMERIC | PROTECTED | UNPROTECTED | DISPLAY | DARK | BRIGHT | DETECT | NOMDT | MDT | BLINK | NOBLINK | REVERSE_VIDEO |
    NORMAL_VIDEO | UNDERSCORE | NOUNDERSCORE | NOCOLOR | BLUE | RED | PINK | GREEN | TURQUOISE | YELLOW | WHITE
    ;

// obtain statement

obtainStatement
   : OBTAIN keepClause? findObtainClause
   ;

// IDMS post statement

postStatement
   : POST ((EVENT generalIdentifier) | (EVENT NAME (generalIdentifier | literal) CLEAR?))
   ;

// IDMS put statement

putStatement
   : PUT (putQueueStatement | putScratchClause)
   ;

putQueueStatement
   : QUEUE (ID (generalIdentifier | literal))? (FIRST | LAST)? idmsDmlFromClause putReturnClause? putRetentionClause?
   ;

putReturnClause
   : RETURN RECORD ID INTO generalIdentifier?
   ;

putRetentionClause
   : RETENTION (generalIdentifier | integerLiteral)
   ;

putScratchClause
   : SCRATCH putAreaIdClause? idmsDmlFromClause putRecordClause? putReturnClause
   ;

putAreaIdClause
   : AREA ID (generalIdentifier | literal)
   ;

putRecordClause
   : RECORD ID (generalIdentifier | integerLiteral) REPLACE?
   ;

// read statement

idmsReadStatement
   : READ readIdmsDcStatement idmsOnClause?
   ;

readIdmsDcStatement
   : readLineFromTerminalClause | readTerminalClause
   ;

readTerminalClause
   : TERMINAL idmsWaitNowaitClause? (BUFFER | (MODIFIED FROM POSITION (generalIdentifier | literal)))?
   (GET STORAGE)? INTO generalIdentifier ((TO generalIdentifier) | (MAX LENGTH (generalIdentifier | integerLiteral)))
   (RETURN LENGTH INTO? generalIdentifier)?
   ;

readLineFromTerminalClause
   : LINE FROM? TERMINAL ECHO? NOBACKPAGE? INTO generalIdentifier ((TO generalIdentifier)
   | (MAX LENGTH (generalIdentifier | integerLiteral))) (RETURN LENGTH INTO? generalIdentifier)?
   ;

// ready statement
readyStatement
    : READY idms_db_entity_name? (USAGE_MODE IS? (PROTECTED | EXCLUSIVE)? (RETRIEVAL | UPDATE))?
    ;

// return statement

idmsReturnStatement
   : RETURN idmsReturn idmsOnClause?
   ;

idmsReturn
    : generalIdentifier FROM idms_db_entity_name (CURRENCY | orderClause CURRENCY? | USING generalIdentifier)
      (KEY INTO? generalIdentifier)?
    ;

// rollback statement
rollbackStatement
    : ROLLBACK TASK? CONTINUE?
    ;

// send statement

idmsSendStatement
   : SEND sendIdmsClause
   ;

sendIdmsClause
   : MESSAGE (ONLY | ALWAYS)? TO sendIdmsToClause idmsDmlFromClause
   ;

sendIdmsToClause
   : (DEST ID (generalIdentifier | literal)) | (USER ID generalIdentifier) | (LTERM ID (generalIdentifier | literal))
   ;

// set statement

idmsSetStatement
   : SET setIdmsDcStatement idmsOnClause?
   ;

setIdmsDcStatement
   : setAbendExitStatement | setTimerStatement
   ;

setAbendExitStatement
   : ABEND EXIT ((ON? PROGRAM (generalIdentifier | literal)) | OFF)
   ;

setTimerStatement
   : TIMER (setTimerWaitClause | setTimerPostClause | setTimerStartClause | (CANCEL setTimerIdClause))
   ;

setTimerWaitClause
   : WAIT setTimerIntervalClause
   ;

setTimerPostClause
   : POST setTimerIntervalClause setTimerEventClause? setTimerIdClause?
   ;

setTimerStartClause
   : START setTimerIntervalClause (TASK CODE (generalIdentifier | literal) (PRIORITY (generalIdentifier | integerLiteral))?)? setTimerIdClause? setTimerDataClause?
   ;

setTimerIntervalClause
   : INTERVAL (generalIdentifier | integerLiteral) SECONDS?
   ;

setTimerEventClause
   : EVENT generalIdentifier
   ;

setTimerIdClause
   : TIMER ID generalIdentifier
   ;

setTimerDataClause
   : DATA idmsDmlFromClause
   ;

// snap statement

snapStatement
   : SNAP (TITLE IS? generalIdentifier)? (ALL | SYSTEM | TASK)? idmsDmlFromClause*
   ;

// startpage statement

startpageStatement
    : STARTPAGE SESSION? idms_map_name (WAIT | NOWAIT | RETURN)? (BACKPAGE | NOBACKPAGE)? (UPDATE | BROWSE)?
     (AUTODISPLAY | NOAUTODISPLAY)?
    ;

// store statement
storeStatement
    : STORE idms_db_entity_name
    ;

// transfer statement

transferStatement
   : TRANSFER CONTROL? TO? (generalIdentifier | idms_program_name) (RETURN | LINK | NORETURN | XCTL)?
   (USING generalIdentifier (COMMACHAR? generalIdentifier)*?)?
   ;

// wait statement

waitStatement
   : WAIT (((LONG | SHORT)? (waitEventTypeClause | waitEventListClause (COMMACHAR? waitEventListClause)*))
   | (REDISPATCH (waitEventTypeClause | waitEventListClause (COMMACHAR? waitEventListClause)*)?))
   ;

waitEventTypeClause
   : EVENT NAME (generalIdentifier | literal)
   ;

waitEventListClause
   : EVENT generalIdentifier
   ;







idms_map_name
    : {validateLength(_input.LT(1).getText(), "map name", 8);} variableUsageName
    ;

idms_map_name_definition
    : {validateLength(_input.LT(1).getText(), "map name", 8);} dataName
    ;

idms_db_entity_name
    : {validateLength(_input.LT(1).getText(), "db entity name", 16);} variableUsageName
    ;

idms_dictionary_name
    : {validateLength(_input.LT(1).getText().substring(1, _input.LT(1).getText().length() -1),
     "dictionary name", 8);} literal
    ;

idms_node_name
    : {validateLength(_input.LT(1).getText().substring(1, _input.LT(1).getText().length() -1),
           "node name", 8);} literal
    ;

idms_procedure_name
    : {validateLength(_input.LT(1).getText(), "procedure name", 8);} variableUsageName
    ;

idms_program_name
    : {validateLength(_input.LT(1).getText().substring(1, _input.LT(1).getText().length() -1),
           "program name", 8);} literal
    ;

idms_schema_name
    : {validateLength(_input.LT(1).getText(), "schema name", 8);} dataName
    ;

idms_subschema_name
    : {validateLength(_input.LT(1).getText(), "subschema name", 8);} dataName
    ;

idms_table_name
    : {validateLength(_input.LT(1).getText().substring(1, _input.LT(1).getText().length() -1),
           "table name", 8);} literal
    ;

// identifier ----------------------------------

generalIdentifier
   : specialRegister | qualifiedDataName | functionCall
   ;

functionCall
   : FUNCTION functionName (LPARENCHAR argument (COMMACHAR? argument)* RPARENCHAR)* referenceModifier?
   ;

referenceModifier
   : LPARENCHAR characterPosition COLONCHAR length? RPARENCHAR
   ;

characterPosition
   : arithmeticExpression
   ;

length
   : arithmeticExpression
   ;

argument
   : arithmeticExpression
   ;

// qualified data name ----------------------------------

qualifiedDataName
   : variableUsageName tableCall? referenceModifier? inData*
   ;

tableCall
   : LPARENCHAR (ALL | arithmeticExpression) (COMMACHAR? (ALL | arithmeticExpression))* RPARENCHAR
   ;

specialRegister
   : ADDRESS OF generalIdentifier
   | DATE | DAY | DAY_OF_WEEK | DEBUG_CONTENTS | DEBUG_ITEM | DEBUG_LINE | DEBUG_NAME | DEBUG_SUB_1 | DEBUG_SUB_2 | DEBUG_SUB_3
   | JNIENVPTR
   | LENGTH OF? generalIdentifier | LINAGE_COUNTER | LINE_COUNTER
   | PAGE_COUNTER
   | RETURN_CODE
   | SHIFT_IN | SHIFT_OUT | SORT_CONTROL | SORT_CORE_SIZE | SORT_FILE_SIZE | SORT_MESSAGE | SORT_MODE_SIZE | SORT_RETURN
   | TALLY | TIME
   | WHEN_COMPILED
   ;

// in ----------------------------------

inData
   : (IN | OF) variableUsageName tableCall? referenceModifier?
   ;

inSection
   : (IN | OF) sectionName
   ;

// names ----------------------------------

alphabetName
   : cobolWord
   ;

assignmentName
   : systemName
   ;

cdName
   : cobolWord
   ;

className
   : cobolWord
   ;

computerName
   : systemName
   ;

dataName
   : cobolWord
   ;

variableUsageName
   : cobolWord
   ;

environmentName
   : systemName
   ;

fileName
   : cobolWord
   ;

functionName
   : INTEGER | LENGTH | RANDOM | SUM | WHEN_COMPILED | cobolWord
   ;

indexName
   : cobolWord
   ;

libraryName
   : cobolWord
   ;

mnemonicName
   : cobolWord
   ;

paragraphName
   : cobolWord | integerLiteral
   ;

paragraphDefinitionName
   : cobolWord | integerLiteral
   ;

procedureName
   : paragraphName inSection?
   ;

programName
   : literal | cobolWord | OR | AND
   ;

recordName
   : qualifiedDataName
   ;

reportName
   : qualifiedDataName
   ;

sectionName
   : cobolWord | integerLiteral
   ;

systemName
   : cobolWord
   ;

symbolicCharacter
   : cobolWord
   ;

figurativeConstant
   : ALL literal | HIGH_VALUE | HIGH_VALUES | LOW_VALUE | LOW_VALUES | NULL | NULLS | QUOTE | QUOTES | SPACE | SPACES | ZEROS | ZEROES
   ;

booleanLiteral
   : TRUE | FALSE
   ;

numericLiteral
   : NUMERICLITERAL | ZERO | integerLiteral
   ;

integerLiteral
   : INTEGERLITERAL | LEVEL_NUMBER | LEVEL_NUMBER_66 | LEVEL_NUMBER_77 | LEVEL_NUMBER_88
   ;

cicsDfhRespLiteral
   : DFHRESP LPARENCHAR (cics_conditions | cobolWord | literal) RPARENCHAR
   ;

cicsDfhValueLiteral
   : DFHVALUE LPARENCHAR (cics_conditions | cobolWord | literal) RPARENCHAR
   ;

cics_conditions: EOC | EODS | INVMPSZ | INVPARTN | INVREQ | MAPFAIL | PARTNFAIL | RDATT | UNEXPIN;

literal
   : NONNUMERICLITERAL | figurativeConstant | numericLiteral | booleanLiteral | charString | cicsDfhRespLiteral | cicsDfhValueLiteral
   ;

charString
   : FINALCHARSTRING
   ;

endClause
    : (DOT_FS | SEMICOLON_FS)
    ;

// arithmetic expression ----------------------------------

arithmeticExpression
   : multDivs plusMinus*
   ;

plusMinus
   : (PLUSCHAR | MINUSCHAR) multDivs
   ;

multDivs
   : powers multDiv*
   ;

multDiv
   : (ASTERISKCHAR | SLASHCHAR) powers
   ;

powers
   : (PLUSCHAR | MINUSCHAR)? basis power*
   ;

power
   : DOUBLEASTERISKCHAR basis
   ;

basis
   : generalIdentifier | literal | LPARENCHAR arithmeticExpression RPARENCHAR
   ;

cobolWord
   : IDENTIFIER | idms_only_words | cobolCompilerDirectivesKeywords | cobolKeywords
   ;

cobolKeywords
   : ABEND | ADDRESS | BOTTOM | BUFFER | CHECK | COUNT | CR | FIELD | FIRST | HEADER | LINK | MMDDYYYY | PRINTER
   | REMARKS | RESUME | TIMER | TODAYS_DATE | TODAYS_NAME | TOP | UPDATE | YEAR | YYYYDDD | YYYYMMDD
   ;

cobolCompilerDirectivesKeywords
   : ADATA | ADV | ANSI | APOST | AR | ARITH | AWO | ALIAS | ANY | AUTO
   | BIN | BLOCK0 | BUF | BUFSIZE
   | C_CHAR | CBLCARD | CO | COBOL2 | COBOL3 | CODEPAGE | COMPAT | COMPILE | CP | CPP | CPSM | CICS | CS | CURR | CURRENCY
   | D_CHAR | DATEPROC | DBCS | DD | DEBUG | DECK | DIAGTRUNC | DLL | DP | DTR | DU | DUMP | DYNAM | DYN
   | E_CHAR | EDF | EJPD | EN | ENGLISH | EPILOG | EXTEND | EXIT | EXP | EXPORTALL
   | F_CHAR | FASTSRT | FEPI | FLAG | FLAGSTD | FSRT | FULL
   | GDS | GRAPHIC
   | H_CHAR | HOOK
   | I_CHAR | INTDATE
   | JA | JP
   | KA
   | LANG | LANGUAGE | LC | LEASM | LILIAN | LIN | LINECOUNT | LIST | LM | LONGMIXED | LONGUPPER | LU
   | M_CHAR | MAP | MARGINS | MAX | MDECK | MD | MIG | MIXED
   | N_CHAR | NAME | NAT | NATLANG | NN | NS | NSEQ | NSYMBOL
   | NOALIAS | NOADATA | NOADV | NOAWO
   | NOBLOCK0
   | NOC | NOCOMPILE | NOCBLCARD | NOCICS | NOCMPR2 | NOCPSM | NOCURRENCY | NOCURR
   | NODATEPROC | NODP | NODBCS | NODEBUG | NODECK | NOD | NODLL | NODE| NODUMP | NODU | NODIAGTRUNC | NODTR | NODYNAM | NODYN
   | NOEDF | NOEPILOG | NOEXIT | NOEXPORTALL | NOEXP | NOEJPD
   | NOFLAG | NOFASTSRT | NOFSRT | NOFEPI | NOF | NOFLAGMIG | NOFLAGSTD
   | NOGRAPHIC
   | NOHOOK
   | NOLENGTH | NOLIB | NOLINKAGE | NOLIST
   | NOMAP | NOMDECK | NOMD | NONUMBER | NONUM
   | NONAME
   | NOOBJECT | NOOBJ | NOOFFSET | NOOFF | NOOPSEQUENCE | NOOPTIMIZE | NOOPT | NOOPTIONS | NOP
   | NOPROLOG | NOPFD
   | NORENT
   | NOSEQUENCE | NOSEQ | NOSOURCE | NOS | NOSPIE | NOSQL | NOSQLCCSID | NOSQLC | NOSSRANGE | NOSSR | NOSTDTRUNC
   | NOTRIG | NOTERMINAL | NOTERM | NOTEST | NOTHREAD
   | NOVBREF
   | NOWORD | NOWD
   | NOXREF | NOX
   | NOZWB
   | NUMBER | NUM | NUMPROC
   | OBJECT | OBJ | OFFSET | OFF | OPMARGINS | OPSEQUENCE | OPTIMIZE | OPT | OPTFILE | OPTIONS | OP | OUTDD | OUT
   | PFD | PGMNAME | PGMN | PROLOG
   | RENT | RES | RMODE
   | S_CHAR | SS | SP | SZ | STD | SSR | SEQ | SEP
   | SOURCE | SPIE | SQLCCSID | SQLC | SSRANGE | SYSEIB | SEQUENCE| SIZE | SEPARATE | SHORT
   | Q_CHAR | QUOTE
   | TRIG | TERMINAL | TERM | TEST | THREAD | TRUNC
   | U_CHAR | UE | UPPER
   | VBREF
   | W_CHAR | WORD | WD
   | X_CHAR | XMLPARSE | XMLSS | XP | XREF
   | YEARWINDOW | YW
   | ZWB
   ;

// to delete
idms_only_words
    : ATTRIBUTE | AUTODISPLAY
    | BACKPAGE | BACKSCAN | BLINK
    | BLUE | BRIGHT | BROWSE | BUT
    | CALC | CONTENTS | COPIES | CORRECT
    | DARK | DATASTREAM
    | DBNAME | DBNODE | DB_KEY
    | DC | DEQUEUE | DEST | DETECT | DFLD
    | DICTNAME | DICTNODE | DIFFERENT | DUPLICATE
    | EAU | ECHO | EDIT | EIGHTYCR | ENDPAGE | ENDRPT
    | ENQUEUE | EXITS | EXTRANEOUS
    | FIELDS | FIND | FORTYCR
    | GREEN
    | IDENTICAL | IDMS | INTERNAL
    | JOURNAL
    | LOADLIB | LOCATION | LOG | LONGTERM
    | MAPS | MDT | MEMBERS
    | MODIFIED | MODIFY
    | NEWPAGE | NOALARM | NOAUTODISPLAY
    | NOBACKPAGE | NOBACKSCAN | NOBLINK | NOCOLOR | NODEADLOCK
    | NODENAME | NOIO | NOKBD | NOLOCK | NOMDT
    | NOPRT | NORETURN | NORMAL
    | NORMAL_VIDEO | NOSPAN | NOTIFICATION | NOTIFY | NOUNDERSCORE
    | OBTAIN | OUTIN | OWNER
    | PAGE_INFO | PARMS | PERMANENT | PINK | PROTECTED
    | RED | REDISPATCH | RESETKBD | RESETMDT | RETENTION | RETRIEVAL | REPLY
    | RETURNKEY | REVERSE_VIDEO | RUN_UNIT
    | SCREEN | SELECTIVE | SHORT | SIXTYFOURCR | SPAN | SCRATCH
    | STARTPAGE | STARTPRT | STGID | STORE | SCHEMA
    | TURQUOISE
    | UNDERSCORE | UNFORMATTED | UNPROTECTED
    | UPGRADE | USAGE_MODE
    | WCC | WHITE | WITHIN | YELLOW
    ;

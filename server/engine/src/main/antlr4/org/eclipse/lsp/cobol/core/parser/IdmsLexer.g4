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

lexer grammar IdmsLexer;

channels{COMMENTS}
import TechnicalLexer;

ABEND : A B E N D;
ABORT : A B O R T;
ACCEPT: A C C E P T;
ACTIVE : A C T I V E;
ADDRESS: A D D R E S S;
AID : A I D;
ALARM : A L A R M;
ALL: A L L;
ALPHANUMERIC: A L P H A N U M E R I C;
ALWAYS : A L W A Y S;
ANY: A N Y;
AREA: A R E A;
AT: A T;
ATTACH: A T T A C H;
ATTRIBUTE : A T T R I B U T E;
ATTRIBUTES: A T T R I B U T E S;
AUTODISPLAY : A U T O D I S P L A Y;
BACKPAGE : B A C K P A G E;
BACKSCAN : B A C K S C A N;
BELOW: B E L O W;
BIND : B I N D;
BLINK : B L I N K;
BLUE : B L U E;
BRIGHT : B R I G H T;
BROWSE : B R O W S E;
BUFFER: B U F F E R;
BUT : B U T;
BY: B Y;
CALC : C A L C;
CANCEL: C A N C E L;
CHANGE: C H A N G E;
CHANGED: C H A N G E D;
CHECK: C H E C K;
CLASS: C L A S S;
CLEAR: C L E A R;
CODE: C O D E;
COMMIT: C O M M I T;
COMP_3 : C O M P MINUSCHAR '3';
COMP: C O M P;
CONNECT: C O N N E C T;
CONTENTS : C O N T E N T S;
CONTINUE : C O N T I N U E;
CONTROL: C O N T R O L;
COPIES : C O P I E S;
COPY : C O P Y;
CORRECT : C O R R E C T;
CR : C R;
CURRENCY: C U R R E N C Y;
CURRENT: C U R R E N T;
CURSOR: C U R S O R;
DARK : D A R K;
DATA: D A T A;
DATASTREAM : D A T A S T R E A M;
DATE: D A T E;
DAY: D A Y;
DAY_OF_WEEK : D A Y MINUSCHAR O F MINUSCHAR W E E K;
DB: D B;
DB_KEY : D B MINUSCHAR K E Y;
DBNAME : D B N A M E;
DBNODE : D B N O D E;
DC : D C;
DEBUG: D E B U G;
DELETE: D E L E T E;
DEQUEUE : D E Q U E U E;
DEST : D E S T;
DESTINATION: D E S T I N A T I O N;
DETAIL: D E T A I L;
DETECT : D E T E C T;
DFLD : D F L D;
DICTNAME : D I C T N A M E;
DICTNODE : D I C T N O D E;
DIFFERENT : D I F F E R E N T;
DISCONNECT: D I S C O N N E C T;
DISPLAY: D I S P L A Y;
DUMP: D U M P;
DUPLICATE : D U P L I C A T E;
EAU : E A U;
ECHO: E C H O;
EDIT: E D I T;
EIGHTYCR : '8' '0' C R;
EMPTY: E M P T Y;
END: E N D;
ENDPAGE : E N D P A G E;
ENDRPT : E N D R P T;
ENQUEUE : E N Q U E U E;
ERASE: E R A S E;
ERROR: E R R O R;
EVENT : E V E N T;
EXCEPT: E X C E P T;
EXCLUSIVE: E X C L U S I V E;
EXIT: E X I T;
EXITS : E X I T S;
EXTENDED: E X T E N D E D;
EXTERNAL: E X T E R N A L;
EXTRANEOUS : E X T R A N E O U S;
FALSE: F A L S E;
FIELD : F I E L D;
FIELDS : F I E L D S;
FILE : F I L E;
FIND : F I N D;
FINISH: F I N I S H;
FIRST: F I R S T;
FOR: F O R;
FORTYCR : '4' '0' C R;
FREE: F R E E;
FROM: F R O M;
FUNCTION: F U N C T I O N;
GET : G E T;
GREEN : G R E E N;
HEADER: H E A D E R;
HIGH_VALUE: H I G H MINUSCHAR V A L U E;
HIGH_VALUES : H I G H MINUSCHAR V A L U E S;
HOLD: H O L D;
IDENTICAL : I D E N T I C A L;
ID: I D;
IDMS : I D M S;
IDMS_CONTROL : I D M S MINUSCHAR C O N T R O L;
IDMS_RECORDS : I D M S MINUSCHAR R E C O R D S;
IDMS_STATISTICS : I D M S MINUSCHAR S T A T I S T I C S;
IF: I F;
IGNORED: I G N O R E D;
IMMEDIATE : I M M E D I A T E;
INCREMENTED : I N C R E M E N T E D;
IN: I N;
INPUT: I N P U T;
INQUIRE : I N Q U I R E;
INTEGER: I N T E G E R;
INTERNAL : I N T E R N A L;
INTERVAL : I N T E R V A L;
INTO: I N T O;
INVOKED: I N V O K E D;
IO: I O;
IS: I S;
JNIENVPTR: J N I E N V P T R;
JOURNAL : J O U R N A L;
JUSTIFY: J U S T I F Y;
KEEP: K E E P;
KEY: K E Y;
LAST: L A S T;
LEFT: L E F T;
LENGTH: L E N G T H;
LEVELS : L E V E L S;
LINAGE_COUNTER : L I N A G E MINUSCHAR C O U N T E R;
LINE: L I N E;
LINKAGE: L I N K A G E;
LINK: L I N K;
LIST: L I S T;
LITERALS: L I T E R A L S;
LOADLIB : L O A D L I B;
LOAD: L O A D;
LOCATION : L O C A T I O N;
LOCK: L O C K;
LOG : L O G;
LONG: L O N G;
LONGTERM : L O N G T E R M;
LOW_VALUE: L O W MINUSCHAR V A L U E;
LOW_VALUES : L O W MINUSCHAR V A L U E S;
LTERM: L T E R M;
MANUAL: M A N U A L;
MAP : M A P;
MAP_CONTROL : M A P MINUSCHAR C O N T R O L;
MAX: M A X;
MDT : M D T;
MEMBER: M E M B E R;
MEMBERS : M E M B E R S;
MESSAGE: M E S S A G E;
MODE: M O D E;
MODIFIED : M O D I F I E D;
MODIFY : M O D I F Y;
MODULE : M O  D U L E;
MOVE: M O V E;
NAME: N A M E;
NATIVE: N A T I V E;
NEW: N E W;
NEWPAGE : N E W P A G E;
NEXT : N E X T;
NLCR: N L C R;
NOALARM : N O A L A R M;
NOAUTODISPLAY : N O A U T O D I S P L A Y;
NOBACKPAGE : N O B A C K P A G E;
NOBACKSCAN : N O B A C K S C A N;
NOBLINK : N O B L I N K;
NOCOLOR : N O C O L O R;
NODEADLOCK : N O D E A D L O C K;
NODENAME : N O D E N A M E;
NODUMP: N O D U M P;
NOIO : N O I O;
NOKBD : N O K B D;
NOLOCK : N O L O C K;
NOMDT : N O M D T;
NONE: N O N E;
NO: N O;
NOPRT : N O P R T;
NORETURN : N O R E T U R N;
NORMAL : N O R M A L;
NORMAL_VIDEO : N O R M A L MINUSCHAR V I D E O;
NOSPAN : N O S P A N;
NOTIFICATION: N O T I F I C A T I O N;
NOTIFY : N O T I F Y;
NOT: N O T;
NOUNDERSCORE : N O U N D E R S C O R E;
NOWAIT: N O W A I T;
NOWRITE: N O W R I T E;
NULL: N U L L;
NULLS: N U L L S;
NUMBER: N U M B E R;
NUMERIC: N U M E R I C;
OBTAIN : O B T A I N;
OFF: O F F;
OF: O F;
ONLY: O N L Y;
ON: O N;
OPTIONAL: O P T I O N A L;
OUTIN : O U T I N;
OUT: O U T;
OUTPUT: O U T P U T;
OWNER : O W N E R;
PAD: P A D;
PAGE_INFO : P A G E MINUSCHAR I N F O;
PAGE: P A G E;
PAGING: P A G I N G;
PARMS : P A R M S;
PERMANENT: P E R M A N E N T;
PINK : P I N K;
POINTER: P O I N T E R;
POSITION: P O S I T I O N;
POST: P O S T;
PREFIX: P R E F I X;
PRINTER: P R I N T E R;
PRIORITY: P R I O R I T Y;
PRIOR : P R I O R;
PROCEDURE: P R O C E D U R E;
PROGRAM: P R O G R A M;
PROTECTED : P R O T E C T E D;
PROTOCOL : P R O T O C O L;
PTERM: P T E R M;
PUT: P U T;
QUEUE: Q U E U E;
QUOTE : Q U O T E;
QUOTES : Q U O T E S;
RANDOM: R A N D O M;
READ: R E A D;
READY: R E A D Y;
RECORD: R E C O R D;
REDEFINES : R E D E F I N E S;
REDISPATCH : R E D I S P A T C H;
RED : R E D;
RELEASE: R E L E A S E;
REPLACE: R E P L A C E;
REPLY : R E P L Y;
REPORT: R E P O R T;
REQUIRED: R E Q U I R E D;
RESETKBD : R E S E T K B D;
RESETMDT : R E S E T M D T;
RESUME: R E S U M E;
RETENTION : R E T E N T I O N;
RETRIEVAL : R E T R I E V A L;
RETURNKEY : R E T U R N K E Y;
RETURN: R E T U R N;
REVERSE_VIDEO : R E V E R S E MINUSCHAR V I D E O;
RIGHT: R I G H T;
ROLLBACK: R O L L B A C K;
RUN_UNIT : R U N MINUSCHAR U N I T;
SCHEMA : S C H E M A;
SCRATCH : S C R A T C H;
SCREEN : S C R E E N;
SCREENSIZE: S C R E E N S I Z E;
SECONDS: S E C O N D S;
SECTION: S E C T I O N;
SELECTIVE : S E L E C T I V E;
SEND: S E N D;
SENTENCE: S E N T E N C E;
SEQUENCE: S E Q U E N C E;
SESSION: S E S S I O N;
SET: S E T;
SHARED: S H A R E D;
SHARE: S H A R E;
SHORT : S H O R T;
SIXTYFOURCR: '6' '4' C R;
SKIPCHAR: S K I P;
SNAP: S N A P;
SOME: S O M E;
SPACE : S P A C E;
SPACES : S P A C E S;
SPAN : S P A N;
STANDARD: S T A N D A R D;
STARTPAGE: S T A R T P A G E;
STARTPRT : S T A R T P R T;
START: S T A R T;
STATISTICS: S T A T I S T I C S;
STGID : S T G I D;
STORAGE: S T O R A G E;
STORE : S T O R E;
SUBSCHEMA_NAMES : S U B S C H E M A MINUSCHAR N A M E S;
SUM: S U M;
SUPPRESS: S U P P R E S S;
SYSTEM: S Y S T E M;
SYSVERSION: S Y S V E R S I O N;
TABLE: T A B L E;
TALLY: T A L L Y;
TASK: T A S K;
TEMPORARY: T E M P O R A R Y;
TERMINAL: T E R M I N A L;
TEST: T E S T;
TEXT: T E X T;
THEN: T H E N;
TIMEOUT : T I M E O U T;
TIMER : T I M E R;
TIME: T I M E;
TITLE: T I T L E;
TO: T O;
TRANSACTION: T R A N S A C T I O N;
TRANSFER: T R A N S F E R;
TRUE: T R U E;
TRUNCATED: T R U N C A T E D;
TURQUOISE : T U R Q U O I S E;
TYPE : T Y P E;
UNDERSCORE : U N D E R S C O R E;
UNFORMATTED : U N F O R M A T T E D;
UNPROTECTED : U N P R O T E C T E D;
UPDATE: U P D A T E;
UPGRADE : U P G R A D E;
USAGE_MODE : U S A G E MINUSCHAR M O D E;
USER: U S E R;
USING: U S I N G;
VALUE: V A L U E;
VERSION : V E R S I O N;
WAIT: W A I T;
WCC: W C C;
WHEN_COMPILED : W H E N MINUSCHAR C O M P I L E D;
WHITE : W H I T E;
WITHIN : W I T H I N;
WITH: W I T H;
WORKING_STORAGE: W O R K I N G MINUSCHAR S T O R A G E;
WRITE: W R I T E;
XCTL: X C T L;
YELLOW : Y E L L O W;
YES: Y E S;
ZEROES : Z E R O E S;
ZEROS : Z E R O S;
ZERO: Z E R O;

mode PICTURECLAUSE;
FINALCHARSTRING: CHARSTRING+ ->popMode;
CHARSTRING: PICTURECHARSGROUP1+ PICTURECHARSGROUP2? LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)
			PICTURECHARSGROUP1+ PICTURECHARSGROUP2? LParIntegralRPar?|
			PICTURECHARSGROUP1* '.' PICTUREPeriodAcceptables+ LParIntegralRPar?|
			PICTURECHARSGROUP1* PICTURECHARSGROUP2? PICTURECHARSGROUP1+ LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)|
			PICTURECHARSGROUP1* PICTURECHARSGROUP2? PICTURECHARSGROUP1+ LParIntegralRPar?|
			PICTURECHARSGROUP2 PICTURECHARSGROUP1* LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)|
			PICTURECHARSGROUP2 PICTURECHARSGROUP1* LParIntegralRPar?
;

PICTURECHARSGROUP1: PICTURECharAcceptedMultipleTime+;
PICTURECHARSGROUP2: PICTURECharAcceptedOneTime+;
WS2 : [ \t\f]+ -> channel(HIDDEN);
LParIntegralRPar: LPARENCHAR INTEGERLITERAL RPARENCHAR;
fragment PICTUREPeriodAcceptables: ('0'|'9'|B|Z|CR|DB|ASTERISKCHAR|COMMACHAR|MINUSCHAR|PLUSCHAR|SLASHCHAR);
fragment PICTURECharAcceptedMultipleTime: (A|G|N|P|X|DOLLARCHAR|PICTUREPeriodAcceptables);
fragment PICTURECharAcceptedOneTime: (V|E|S|CR|DB);


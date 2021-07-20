/*
 * Copyright (c) 2020 Broadcom.
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
    
lexer grammar CobolLexer;
import CICSLexer;
channels{TECHNICAL}
@header {
  import java.util.regex.Matcher;
  import java.util.regex.Pattern;
  import org.apache.commons.lang3.StringUtils;
}
@lexer::members {
    private static final Pattern titlePattern =
        Pattern.compile("TITLE\\s*(.*?)\\.?\\s\\n.*", Pattern.CASE_INSENSITIVE);
        private static final int ENTER_LENGTH = 5;
    private void checkTitlePresent()
    {
      String input = _input.getText(Interval.of(_tokenStartCharIndex, _input.index()));
      if (input!=null)
      {
        Matcher matcher = titlePattern.matcher(input);
          if(matcher.matches() && matcher.group(1).isEmpty())
           reportWrongArguments("lexer.titleCompilerDirective");
      }
    }
    private void reportWrongArguments(String msg) {
        ANTLRErrorListener listener = getErrorListenerDispatch();
        String text = _input.getText(Interval.of(_tokenStartCharIndex, _input.index()));
	    int stop = _input.index();
	    if(text.contains("\r")) {
		   stop--;
	    }
        CommonToken lspToken = new CommonTokenFactory().create(_tokenFactorySourcePair, _type, text,
	         					_channel, _input.index()-text.length()+1,
	         					stop,
	        					_tokenStartLine, _tokenStartCharPositionInLine);
        lspToken.setTokenIndex(_tokenStartCharPositionInLine);
        lspToken.setText(text.replace("\r","").replace("\n",""));
        listener.syntaxError(this, lspToken, _tokenStartLine,
         	         _tokenStartCharPositionInLine, msg, null);
    }

    private void checkLanguageNamePresent()
    {
      String input = _input
                     .getText(Interval.of(_tokenStartCharIndex, _input.index()))
                     .replace("\\R","").replace(".","").trim();
      if(input.length() <= ENTER_LENGTH || StringUtils.isBlank(input.substring(ENTER_LENGTH, input.length())))
          reportWrongArguments("lexer.langMissingEnterDirective");
    }
}

// compiler directive tokens
TITLESTATEMENT : (TITLE ' '+ .*? NEWLINE) {checkTitlePresent();} -> channel(TECHNICAL);

CONTROL_DIRECTIVE: ASTERISKCHAR (CONTROL | CBL) ((' '| COMMACHAR)
                  (SOURCE | NO SOURCE | LIST | NO LIST | MAP | NO MAP
                  | IDENTIFIER? {reportWrongArguments("lexer.controlDirectiveWrongArgs");})
                  )+ DOT_FS?-> channel(TECHNICAL);

ENTER_STMT: ENTER ' '+ IDENTIFIER? {checkLanguageNamePresent();} (' '+ IDENTIFIER)?  ' '* DOT_FS-> channel(TECHNICAL);
EJECT: E J E C T DOT_FS? -> channel(HIDDEN);
SKIP1 : S K I P '1' DOT_FS? -> skip;
SKIP2 : S K I P '2' DOT_FS? -> skip;
SKIP3 : S K I P '3' DOT_FS?-> skip;

// keywords
ACCEPT : A C C E P T;
ADATA : A D A T A;
ADV : A D V;
ADVANCING : A D V A N C I N G;
ALIGNED : A L I G N E D;
ALPHABET : A L P H A B E T;
ALPHABETIC : A L P H A B E T I C;
ALPHABETIC_LOWER : A L P H A B E T I C MINUSCHAR L O W E R;
ALPHABETIC_UPPER : A L P H A B E T I C MINUSCHAR U P P E R;
ALPHANUMERIC : A L P H A N U M E R I C;
ALPHANUMERIC_EDITED : A L P H A N U M E R I C MINUSCHAR E D I T E D;
ALSO : A L S O;
ALTERNATE : A L T E R N A T E;
ANSI : A N S I;
APPLY : A P P L Y;
APOST : A P O S T;
AREA : A R E A;
AREAS : A R E A S;
ARITH : A R I T H;
ASCENDING : A S C E N D I N G;
ASSOCIATED_DATA : A S S O C I A T E D MINUSCHAR D A T A;
ASSOCIATED_DATA_LENGTH : A S S O C I A T E D MINUSCHAR D A T A MINUSCHAR L E N G T H;
ATTRIBUTE : A T T R I B U T E;
AUTHOR : A U T H O R;
AUTO : A U T O;
AUTODISPLAY : A U T O D I S P L A Y;
AUTO_SKIP : A U T O MINUSCHAR S K I P;
AWO : A W O;
BACKGROUND_COLOR : B A C K G R O U N D MINUSCHAR C O L O R;
BACKGROUND_COLOUR : B A C K G R O U N D MINUSCHAR C O L O U R;
BACKPAGE : B A C K P A G E;
BACKSCAN : B A C K S C A N;
BASIS : B A S I S;
BEGINNING : B E G I N N I N G;
BIN : B I N;
BLANK : B L A N K;
BLINK : B L I N K;
BLOCK : B L O C K;
BLOCK0 : B L O C K '0';
BLUE : B L U E;
BOTTOM : B O T T O M;
BOUNDS : B O U N D S;
BRIGHT : B R I G H T;
BROWSE : B R O W S E;
BUF : B U F;
BUFSIZE : B U F S I Z E;
BUT : B U T;
BYFUNCTION : B Y F U N C T I O N;
BYTITLE : B Y T I T L E;
CALC : C A L C;
CANCEL : C A N C E L;
CAPABLE : C A P A B L E;
CBL : C B L;
CBLCARD : C B L C A R D;
CCSVERSION : C C S V E R S I O N;
CHAINING : C H A I N I N G;
CHARACTERS : C H A R A C T E R S;
CLASS_ID : C L A S S MINUSCHAR I D;
CLOCK_UNITS : C L O C K MINUSCHAR U N I T S;
CLOSE : C L O S E;
CLOSE_DISPOSITION : C L O S E MINUSCHAR D I S P O S I T I O N;
COBOL2 : C O B O L '2';
COBOL3 : C O B O L '3';
CODE_SET : C O D E MINUSCHAR S E T;
COLLATING : C O L L A T I N G;
COMMA : C O M M A;
COMMITMENT : C O M M I T M E N T;
COMMON : C O M M O N;
COMP : C O M P;
COMPAT : C O M P A T;
COMPILE : C O M P I L E;
COMPUTATIONAL : C O M P U T A T I O N A L;
COMPUTATIONAL_1 : C O M P U T A T I O N A L MINUSCHAR '1';
COMPUTATIONAL_2 : C O M P U T A T I O N A L MINUSCHAR '2';
COMPUTATIONAL_3 : C O M P U T A T I O N A L MINUSCHAR '3';
COMPUTATIONAL_4 : C O M P U T A T I O N A L MINUSCHAR '4';
COMPUTATIONAL_5 : C O M P U T A T I O N A L MINUSCHAR '5';
COMPUTE : C O M P U T E;
COMP_1 : C O M P MINUSCHAR '1';
COMP_2 : C O M P MINUSCHAR '2';
COMP_3 : C O M P MINUSCHAR '3';
COMP_4 : C O M P MINUSCHAR '4';
COMP_5 : C O M P MINUSCHAR '5';
COM_REG : C O M MINUSCHAR R E G;
CONFIGURATION : C O N F I G U R A T I O N;
CONTENT : C O N T E N T;
CONTENTS : C O N T E N T S;
CONTROLS : C O N T R O L S;
CONVENTION : C O N V E N T I O N;
CONVERTING : C O N V E R T I N G;
COPIES : C O P I E S;
COPYENTRY : (' *>CPYENTER<URI>' .*? '</URI>') -> channel(TECHNICAL);
COPYEXIT : '*>CPYEXIT' + NEWLINE -> channel(TECHNICAL);
CORRECT : C O R R E C T;
CPP : C P P;
CPSM : C P S M;
CRUNCH : C R U N C H;
CURR : C U R R;
DARK : D A R K;
DATASTREAM : D A T A S T R E A M;
DATE_COMPILED : D A T E MINUSCHAR C O M P I L E D;
DATE_WRITTEN : D A T E MINUSCHAR W R I T T E N;
DATEPROC : D A T E P R O C;
DAY_OF_WEEK : D A Y MINUSCHAR O F MINUSCHAR W E E K;
DB_KEY : D B MINUSCHAR K E Y;
DBNAME : D B N A M E;
DBNODE : D B N O D E;
DC : D C;
DECK : D E C K;
DEBUGGING : D E B U G G I N G;
DEBUG_CONTENTS : D E B U G MINUSCHAR C O N T E N T S;
DEBUG_ITEM : D E B U G MINUSCHAR I T E M;
DEBUG_LINE : D E B U G MINUSCHAR L I N E;
DEBUG_NAME : D E B U G MINUSCHAR N A M E;
DEBUG_SUB_1 : D E B U G MINUSCHAR S U B MINUSCHAR '1';
DEBUG_SUB_2 : D E B U G MINUSCHAR S U B MINUSCHAR '2';
DEBUG_SUB_3 : D E B U G MINUSCHAR S U B MINUSCHAR '3';
DECIMAL_POINT : D E C I M A L MINUSCHAR P O I N T;
DECLARATIVES : D E C L A R A T I V E S;
DEFAULT_DISPLAY : D E F A U L T MINUSCHAR D I S P L A Y;
DELIMITED : D E L I M I T E D;
DEPENDING : D E P E N D I N G;
DEQUEUE : D E Q U E U E;
DETECT : D E T E C T;
DESCENDING : D E S C E N D I N G;
DEST : D E S T;
DESTINATION : D E S T I N A T I O N;
DFHRESP : D F H R E S P;
DFHVALUE : D F H V A L U E;
DFLD : D F L D;
DIAGTRUNC : D I A G T R U N C;
DICTNAME : D I C T N A M E;
DICTNODE : D I C T N O D E;
DIFFERENT : D I F F E R E N T;
DISK : D I S K;
DISPLAY_1 : D I S P L A Y MINUSCHAR '1';
DIVIDE : D I V I D E;
DIVISION : D I V I S I O N;
DLL : D L L;
DOWN : D O W N;
DTR : D T R;
DUPLICATE : D U P L I C A T E;
DUPLICATES : D U P L I C A T E S;
EDIT: E D I T;
DYN : D Y N;
DYNAM : D Y N A M;
EAU : E A U;
ECHO: E C H O;
EDF : E D F;
EGCS : E G C S;
EGI : E G I;
EIGHTYCR : '8' '0' C R;
EJPD : E J P D;
EMI : E M I;
ENCODING: E N C O D I N G;
END_ACCEPT : E N D MINUSCHAR A C C E P T;
END_ADD : E N D MINUSCHAR A D D;
END_CALL : E N D MINUSCHAR C A L L;
END_COMPUTE : E N D MINUSCHAR C O M P U T E;
END_DELETE : E N D MINUSCHAR D E L E T E;
END_DIVIDE : E N D MINUSCHAR D I V I D E;
END_EVALUATE : E N D MINUSCHAR E V A L U A T E;
END_IF : E N D MINUSCHAR I F;
END_MULTIPLY : E N D MINUSCHAR M U L T I P L Y;
END_OF_PAGE : E N D MINUSCHAR O F MINUSCHAR P A G E;
END_PERFORM : E N D MINUSCHAR P E R F O R M;
END_READ : E N D MINUSCHAR R E A D;
END_RECEIVE : E N D MINUSCHAR R E C E I V E;
END_RETURN : E N D MINUSCHAR R E T U R N;
END_REWRITE : E N D MINUSCHAR R E W R I T E;
END_SEARCH : E N D MINUSCHAR S E A R C H;
END_START : E N D MINUSCHAR S T A R T;
END_STRING : E N D MINUSCHAR S T R I N G;
END_SUBTRACT : E N D MINUSCHAR S U B T R A C T;
END_UNSTRING : E N D MINUSCHAR U N S T R I N G;
END_WRITE : E N D MINUSCHAR W R I T E;
END_XML : E N D MINUSCHAR X M L;
ENDPAGE : E N D P A G E;
ENDRPT : E N D R P T;
ENGLISH : E N G L I S H;
ENQUEUE : E N Q U E U E;
EOP : E O P;
EPILOG : E P I L O G;
ESI : E S I;
EVALUATE : E V A L U A T E;
EVERY : E V E R Y;
EXHIBIT : E X H I B I T;
EXITS : E X I T S;
EXP : E X P;
EXPORTALL : E X P O R T A L L;
EXTRANEOUS : E X T R A N E O U S;
FASTSRT : F A S T S R T;
FEPI : F E P I;
FD : F D;
FIELDS : F I E L D S;
FILE_CONTROL : F I L E MINUSCHAR C O N T R O L;
FILLER : F I L L E R;
FINISH : F I N I S H;
FIND : F I N D;
FLAG : F L A G;
FLAGSTD : F L A G S T D;
FOOTING : F O O T I N G;
FORTYCR : '4' '0' C R;
FSRT : F S R T;
FUNCTION_POINTER : F U N C T I O N MINUSCHAR P O I N T E R;
GIVING : G I V I N G;
GOBACK : G O B A C K;
GREATER : G R E A T E R;
GREEN : G R E E N;
GROUP_USAGE: G R O U P MINUSCHAR U S A G E;
HIGHLIGHT : H I G H L I G H T;
HIGH_VALUE : H I G H MINUSCHAR V A L U E;
HIGH_VALUES : H I G H MINUSCHAR V A L U E S;
HOOK : H O O K;
IDENTICAL : I D E N T I C A L;
IDENTIFICATION : I D E N T I F I C A T I O N;
IDMS : I D M S;
IDMS_CONTROL : I D M S MINUSCHAR C O N T R O L;
IDMS_RECORDS : I D M S MINUSCHAR R E C O R D S;
IDMS_STATISTICS : I D M S MINUSCHAR S T A T I S T I C S;
IF : I F;
IGNORED : I G N O R E D;
IMPLICIT : I M P L I C I T;
INCREMENTED : I N C R E M E N T E D;
INDEX : I N D E X;
INDEXED : I N D E X E D;
INDICATE : I N D I C A T E;
INITIAL : I N I T I A L;
INITIALIZE : I N I T I A L I Z E;
INITIATE : I N I T I A T E;
INPUT_OUTPUT : I N P U T MINUSCHAR O U T P U T;
INSPECT : I N S P E C T;
INSTALLATION : I N S T A L L A T I O N;
INTDATE : I N T D A T E;
INTERNAL : I N T E R N A L;
INVOKED : I N V O K E D;
IO : I O;
I_O : I MINUSCHAR O;
I_O_CONTROL : I MINUSCHAR O MINUSCHAR C O N T R O L;
JOURNAL : J O U R N A L;
JUST : J U S T;
JUSTIFIED : J U S T I F I E D;
KANJI : K A N J I;
KEPT : K E P T;
KEYBOARD : K E Y B O A R D;
LANG : L A N G;
LEASM : L E A S M;
LESS : L E S S;
LEVELS : L E V E L S;
LIB : L I B;
LIBRARY : L I B R A R Y;
LILIAN : L I L I A N;
LINAGE : L I N A G E;
LINAGE_COUNTER : L I N A G E MINUSCHAR C O U N T E R;
LIN : L I N;
LINES : L I N E S;
LINE_COUNTER : L I N E MINUSCHAR C O U N T E R;
LINECOUNT : L I N E C O U N T;
LOADLIB : L O A D L I B;
LOCAL_STORAGE : L O C A L MINUSCHAR S T O R A G E;
LOG : L O G;
LONG_DATE : L O N G MINUSCHAR D A T E;
LONG_TIME : L O N G MINUSCHAR T I M E;
LONGMIXED : L O N G M I X E D;
LONGTERM : L O N G T E R M;
LONGUPPER : L O N G U P P E R;
LOWER : L O W E R;
LOW_VALUE : L O W MINUSCHAR V A L U E;
LOW_VALUES : L O W MINUSCHAR V A L U E S;
LTERM : L T E R M;
LR : L R;
MAID : M A I D;
MAPS : M A P S;
MAP_BINDS : MAP MINUSCHAR B I N D S;
MANUAL : M A N U A L;
MARGINS : M A R G I N S;
MDECK : M D E C K;
MDT : M D T;
MEMBERS : M E M B E R S;
MEMORY : M E M O R Y;
MIG : M I G;
MODIFIED : M O D I F I E D;
MODIFY : M O D I F Y;
MODULES : M O D U L E S;
MORE_LABELS : M O R E MINUSCHAR L A B E L S;
MULTIPLE : M U L T I P L E;
MULTIPLY : M U L T I P L Y;
NAT : N A T;
NATIONAL_EDITED : N A T I O N A L MINUSCHAR E D I T E D;
NATIVE : N A T I V E;
NEGATIVE : N E G A T I V E;
NETWORK : N E T W O R K;
NEWPAGE : N E W P A G E;
NLCR : N L C R;
NOADATA : N O A D A T A;
NOADV : N O A D V;
NOALARM : N O A L A R M;
NOALIAS : N O A L I A S;
NOAUTODISPLAY : N O A U T O D I S P L A Y;
NOAWO : N O A W O;
NOBACKPAGE : N O B A C K P A G E;
NOBACKSCAN : N O B A C K S C A N;
NOBLINK : N O B L I N K;
NOBLOCK0 : N O B L O C K '0';
NOC : N O C;
NOCBLCARD : N O C B L C A R D;
NOCICS : N O C I C S;
NOCMPR2 : N O C M P R '2';
NOCOLOR : N O C O L O R;
NOCOMPILE : N O C O M P I L E;
NOCPSM : N O C P S M;
NOCURR : N O C U R R;
NOCURRENCY : N O C U R R E N C Y;
NOD : N O D;
NODATEPROC : N O D A T E P R O C;
NODBCS : N O D B C S;
NODEADLOCK : N O D E A D L O C K;
NODEBUG : N O D E B U G;
NODECK : N O D E C K;
NODENAME : N O D E N A M E;
NODIAGTRUNC : N O D I A G T R U N C;
NODLL : N O D L L;
NODP : N O D P;
NODTR : N O D T R;
NODU : N O D U;
NODYN : N O D Y N;
NODYNAM : N O D Y N A M;
NOEDF : N O E D F;
NOEJPD : N O E J P D;
NOEPILOG : N O E P I L O G;
NOEXIT : N O E X I T;
NOEXP : N O E X P;
NOEXPORTALL : N O E X P O R T A L L;
NOF : N O F;
NOFASTSRT : N O F A S T S R T;
NOFEPI : N O F E P I;
NOFLAG : N O F L A G;
NOFLAGMIG : N O F L A G M I G;
NOFLAGSTD : N O F L A G S T D;
NOFSRT : N O F S R T;
NOGRAPHIC : N O G R A P H I C;
NOHOOK : N O H O O K;
NOIO : N O I O;
NOKBD : N O K B D;
NOLENGTH : N O L E N G T H;
NOLIB : N O L I B;
NOLINKAGE : N O L I N K A G E;
NOLIST : N O L I S T;
NOLOCK : N O L O C K;
NOMAP : N O M A P;
NOMD : N O M D;
NOMDT : N O M D T;
NOMDECK : N O M D E C K;
NONAME : N O N A M E;
NONUM : N O N U M;
NONUMBER : N O N U M B E R;
NOOBJ : N O O B J;
NOOBJECT : N O O B J E C T;
NOOFF : N O O F F;
NOOFFSET : N O O F F S E T;
NOOPSEQUENCE : N O O P S E Q U E N C E;
NOOPT : N O O P T;
NOOPTIMIZE : N O O P T I M I Z E;
NOOPTIONS : N O O P T I O N S;
NOP : N O P;
NOPFD : N O P F D;
NOPROLOG : N O P R O L O G;
NOPRT : N O P R T;
NORENT : N O R E N T;
NORETURN : N O R E T U R N;
NORMAL : N O R M A L;
NORMAL_VIDEO : N O R M A L MINUSCHAR V I D E O;
NOS : N O S;
NOSEP : N O S E P;
NOSEPARATE : N O S E P A R A T E;
NOSEQ : N O S E Q;
NOSOURCE : N O S O U R C E;
NOSPAN : N O S P A N;
NOSPIE : N O S P I E;
NOSQL : N O S Q L;
NOSQLC : N O S Q L C;
NOSQLCCSID : N O S Q L C C S I D;
NOSSR : N O S S R;
NOSSRANGE : N O S S R A N G E;
NOSTDTRUNC : N O S T D T R U N C;
NOSEQUENCE : N O S E Q U E N C E;
NOTERM : N O T E R M;
NOTERMINAL : N O T E R M I N A L;
NOTEST : N O T E S T;
NOTIFICATION: N O T I F I C A T I O N;
NOTIFY : N O T I F Y;
NOTHREAD : N O T H R E A D;
NOTRIG : N O T R I G;
NOUNDERSCORE : N O U N D E R S C O R E;
NOVBREF : N O V B R E F;
NOWD : N O W D;
NOWORD : N O W O R D;
NOWRITE : N O W R I T E;
NOX : N O X;
NOXREF : N O X R E F;
NOZWB : N O Z W B;
NSEQ : N S E Q;
NSYMBOL : N S Y M B O L;
NUM : N U M;
NUMERIC_DATE : N U M E R I C MINUSCHAR D A T E;
NUMERIC_EDITED : N U M E R I C MINUSCHAR E D I T E D;
NUMERIC_TIME : N U M E R I C MINUSCHAR T I M E;
NUMPROC : N U M P R O C;
OBJ : O B J;
OBJECT_COMPUTER : O B J E C T MINUSCHAR C O M P U T E R;
OBTAIN : O B T A I N;
OCCURS : O C C U R S;
ODT : O D T;
OFF : O F F;
OFFSET : O F F S E T;
OMITTED : O M I T T E D;
OPMARGINS : O P M A R G I N S;
OPSEQUENCE : O P S E Q U E N C E;
OPT : O P T;
OPTFILE : O P T F I L E;
ORDERLY : O R D E R L Y;
OTHER : O T H E R;
OUTDD : O U T D D;
OUTIN : O U T I N;
OVERFLOW : O V E R F L O W;
OWN : O W N;
PACKED_DECIMAL : P A C K E D MINUSCHAR D E C I M A L;
PADDING : P A D D I N G;
PAGE_COUNTER : P A G E MINUSCHAR C O U N T E R;
PAGE_INFO : P A G E MINUSCHAR I N F O;
PARMS : P A R M S;
PARSE: P A R S E;
PERFORM : P E R F O R M;
PERMANENT: P E R M A N E N T;
PFD : P F D;
PGMN : P G M N;
PGMNAME : P G M N A M E;
PIC : P I C  -> pushMode(PICTURECLAUSE);
PICTURE : P I C T U R E -> pushMode(PICTURECLAUSE);
PINK : P I N K;
POINTER : P O I N T E R;
POINTER_32 : P O I N T E R MINUSCHAR '3' '2';
PORT : P O R T;
POSITIVE : P O S I T I V E;
PRINTER : P R I N T E R;
PRINTING : P R I N T I N G;
PROCEDURES : P R O C E D U R E S;
PROCEDURE_POINTER : P R O C E D U R E MINUSCHAR P O I N T E R;
PROCEED : P R O C E E D;
PROCESSING: P R O C E S S I N G;
PROGRAM_ID : P R O G R A M MINUSCHAR I D;
PROLOG : P R O L O G;
PROTECTED : P R O T E C T E D;
PTERM : P T E R M;
PURGE : P U R G E;
QUEUE : Q U E U E;
QUOTES : Q U O T E S;
RANDOM : R A N D O M;
READER : R E A D E R;
READY : R E A D Y;
RECEIVED : R E C E I V E D;
RECORDING : R E C O R D I N G;
RECURSIVE : R E C U R S I V E;
RED : R E D;
REDEFINES : R E D E F I N E S;
REDISPATCH : R E D I S P A T C H;
REEL : R E E L;
RELOAD: R E L O A D;
REMAINDER : R E M A I N D E R;
REMARKS : R E M A R K S;
REMOTE : R E M O T E;
REMOVAL : R E M O V A L;
RENAMES : R E N A M E S;
RENT : R E N T;
REPORT : R E P O R T;
REPORTING : R E P O R T I N G;
REPORTS : R E P O R T S;
RERUN : R E R U N;
RES : R E S;
RESERVE : R E S E R V E;
RESETMDT : R E S E T M D T;
RESETKBD : R E S E T K B D;
RETENTION : R E T E N T I O N;
RETRIEVAL : R E T R I E V A L;
RETURNING: R E T U R N I N G;
RETURNKEY : R E T U R N K E Y;
RETURN_CODE : R E T U R N MINUSCHAR C O D E;
REVERSE_VIDEO : R E V E R S E MINUSCHAR V I D E O;
REVERSED : R E V E R S E D;
RMODE : R M O D E;
ROUNDED : R O U N D E D;
RUN_UNIT : R U N MINUSCHAR U N I T;
SAME : S A M E;
SAVE : S A V E;
SCRATCH : S C R A T C H;
SCREEN : S C R E E N;
SCREENSIZE : S C R E E N S I Z E;
SD : S D;
SEGMENT : S E G M E N T;
SEGMENT_LIMIT : S E G M E N T MINUSCHAR L I M I T;
SELECTIVE : S E L E C T I V E;
SENTENCE : S E N T E N C E;
SEP : S E P;
SEPARATE : S E P A R A T E;
SEQ : S E Q;
SEQUENCE : S E Q U E N C E;
SEQUENTIAL : S E Q U E N T I A L;
SERVICE: S E R V I C E;
SHIFT_IN : S H I F T MINUSCHAR I N;
SHIFT_OUT : S H I F T MINUSCHAR O U T;
SHORT : S H O R T;
SHORT_DATE : S H O R T MINUSCHAR D A T E;
SIGN : S I G N;
SNAP_TITLE : S N A P WS T I T L E;
SNAP : S N A P;
SIXTYFOURCR: '6' '4' C R;
SORT : S O R T;
SORT_CONTROL : S O R T MINUSCHAR C O N T R O L;
SORT_CORE_SIZE : S O R T MINUSCHAR C O R E MINUSCHAR S I Z E;
SORT_FILE_SIZE : S O R T MINUSCHAR F I L E MINUSCHAR S I Z E;
SORT_MERGE : S O R T MINUSCHAR M E R G E;
SORT_MESSAGE : S O R T MINUSCHAR M E S S A G E;
SORT_MODE_SIZE : S O R T MINUSCHAR M O D E MINUSCHAR S I Z E;
SORT_RETURN : S O R T MINUSCHAR R E T U R N;
SOURCE_COMPUTER : S O U R C E MINUSCHAR C O M P U T E R;
SPACES : S P A C E S;
SPAN : S P A N;
SPECIAL_NAMES : S P E C I A L MINUSCHAR N A M E S;
SPIE : S P I E;
STORE : S T O R E;
SQLC : S Q L C;
SQLCCSID : S Q L C C S I D;
SQLIMS : S Q L I M S;
SSR : S S R;
SSRANGE : S S R A N G E;
STARTPAGE: S T A R T P A G E;
STD : S T D;
SUBSCHEMA_NAMES : SSMinusChar N A M E S;
STANDARD : S T A N D A R D;
STANDARD_1 : S T A N D A R D MINUSCHAR '1';
STANDARD_2 : S T A N D A R D MINUSCHAR '2';
STARTPRT : S T A R T P R T;
STGID : S T G I D;
STOP : S T O P;
STRING : S T R I N G;
SSNAMES : S U B S C H E M A MINUSCHAR N A M E S;
SUBTRACT : S U B T R A C T;
SUM : S U M;
SYMBOLIC : S Y M B O L I C;
SYNC : S Y N C;
SYNCHRONIZED : S Y N C H R O N I Z E D;
SYSEIB : S Y S E I B;
SYSVERSION : S Y S V E R S I O N;
TALLY : T A L L Y;
TALLYING : T A L L Y I N G;
TAPE : T A P E;
TERM : T E R M;
TERMINATE : T E R M I N A T E;
THAN : T H A N;
THREAD_LOCAL : T H R E A D MINUSCHAR L O C A L;
THROUGH : T H R O U G H;
THRU : T H R U;
TIMES : T I M E S;
TODAYS_DATE : T O D A Y S MINUSCHAR D A T E;
TODAYS_NAME : T O D A Y S MINUSCHAR N A M E;
TOP : T O P;
TRIG : T R I G;
TRUNC : T R U N C;
TRUNCATED : T R U N C A T E D;
TURQUOISE : T U R Q U O I S E;
TYPEDEF : T Y P E D E F;
UNDERSCORE : U N D E R S C O R E;
UNFORMATTED : U N F O R M A T T E D;
UNIT : U N I T;
UNPROTECTED : U N P R O T E C T E D;
UNSTRING : U N S T R I N G;
UP : U P;
UPGRADE : U P G R A D E;
UPON : U P O N;
USAGE_MODE : U S A G E  MINUSCHAR M O D E;
UTF_8 : U T F MINUSCHAR '8';
VALIDATING: V A L I D A T I N G;
VBREF : V B R E F;
VIRTUAL : V I R T U A L;
WHEN_COMPILED : W H E N MINUSCHAR C O M P I L E D;
WHITE : W H I T E;
WITH : W I T H;
WITHIN : W I T H I N;
WCC: W C C;
WORD : W O R D;
WORDS : W O R D S;
WORKING_STORAGE : W O R K I N G MINUSCHAR S T O R A G E;
WRITE_ONLY : W R I T E MINUSCHAR O N L Y;
XOPTS: X O P T S;
XMLPARSE : X M L P A R S E;
XMLSS : X M L S S;
XREF : X R E F;
YEARWINDOW : Y E A R W I N D O W;
YELLOW : Y E L L O W;
ZWB : Z W B;
ZERO : Z E R O;
ZEROES : Z E R O E S;
ZEROS : Z E R O S;

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

DOT_FS2 : '.' ('\r' | '\n' | '\f' | '\t' | ' ')+ -> popMode;
PICTURECHARSGROUP1: PICTURECharAcceptedMultipleTime+;
PICTURECHARSGROUP2: PICTURECharAcceptedOneTime+;
WS2 : [ \t\f;]+ -> channel(HIDDEN);
IS2: I S;
LParIntegralRPar: LPARENCHAR INTEGERLITERAL RPARENCHAR;
fragment PICTUREPeriodAcceptables: ('0'|'9'|B|Z|CR|DB|ASTERISKCHAR|COMMACHAR|MINUSCHAR|PLUSCHAR|SLASHCHAR);
fragment PICTURECharAcceptedMultipleTime: (A|G|N|P|X|DOLLARCHAR|PICTUREPeriodAcceptables);
fragment PICTURECharAcceptedOneTime: (V|E|S|CR|DB);
fragment SSMinusChar: S U B S C H E M A MINUSCHAR;

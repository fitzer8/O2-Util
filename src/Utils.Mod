MODULE Utils;
(*###############################################################
# MODULE:	Utils.
# FUNCTION:	Contains various routines.
# AUTHOR:	Fitz
# VERS:		1.1
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20021130 # Fitz       # Original.
# 20021228 # Fitz       # Added Int2HexChar and Int2HexStr;
# 20030505 # Fitz       # Added AsciiChar2Hex and AsciiStr2Hex;
# 20030505 # Fitz       # Had to correct a problem where the len
#                       # of a ASCII hex string wasn't reduced
#                       # after a trailing "H" was deleted.
# 20030506 # Fitz       # Turned off Debugging.
# 20030802 # Fitz       # Added UPPER and LOWER.
# 20030827 # Fitz       # Fixed AsciiStr2Hex.
# 20030828 # Fitz       # Removed AsciiStr2Hex and added
#                       # ConvString and AsciiStr2Number.
# 20030915 # Fitz       # Added GetChar.
# 20030916 # Fitz       # Changed Range.
# 20030920 # Fitz       # Corrected grammar in a comment.
# 20031023 # Fitz       # Added IsPrintable.
# 20031109 # Fitz       # Added OrdinalOf, and did some cleanup.
# 20041003 # Fitz       # Added Even and removed Out from IMPORT list.
# 20061210 # Fitz       # Added TBit and SBit.
# 20061210 # Fitz       # Added Spaces.
# 20061210 # Fitz       # Added Out to IMPORT list.
# 20061210 # Fitz       # Exported Spaces.
# 20070107 # Fitz       # Replaced Int2HexStr with Byte2HexStr.
#                       # Delete Int2HexStr at next rebuilding.
# 20070618 # Fitz       # Cleanup after changes to Debug.
# 20070619 # Fitz       # Added Sprintf.
# 20070714 # Fitz       # Added Split.
# 20070716 # Fitz       # Added RBit. Reset bit.
# 20070716 # Fitz       # Cleaned up the Procedures list.
# 20071219 # Fitz       # Updated AsciiStr2Hex.
# 20071219 # Fitz       # Added FileExists.
# 20080412 # Fitz       # Added Unsplit.
# 20080722 # Fitz       # Brought over latest changes from Doze box.
# 20100310 # Fitz       # Added GetLine.
# 20100311 # Fitz       # Added IsAlphaNum.
# 20100727 # Fitz       # Added NewGetByte.
# 20100927 # Fitz       # Added Word2HexStr and cleanup.
# 20101022 # Fitz       # Added TrimWhite.
# 20110101 # Fitz       # Added default to ConvString to be
#												# Decimal.
# 20110101 # Fitz       # Turn off debugging in TrimWhite and
#												# IsAlphaNum.
# 20121206 # Fitz       # Cleanup to match Debug.
# 20121217 # Fitz       # Finished cleanup.
# 20121218 # Fitz       # Finished cleanup to match Debug.
# 20121220 # Fitz       # Cleanup of spacing in AsciiChar2Hex
#							and added a bit to the notes.
# 20121221 # Fitz       # Moved Range to the top so it can be
# 20140514 # Fitz       # Added DumpByte and tBit to replace TBit.
# 20140515 # Fitz       # Added IMPORT of Bits. Updated
#						# dumpByte to call tBit from Bits library.
# 20160916 # Fitz       # Cleanup to compile.
# 20160917 # Fitz       # Added ShowBits.
# 20160918 # Fitz       # Added FileNotFound and FileNotOpen.
#                       # Major clean up.
###############################################################*)

(*###############################################################
# Procedures
PROCEDURE Range*( num, lo, hi: LONGINT ): BOOLEAN;
PROCEDURE LOWER*( VAR str1: ARRAY OF CHAR );
PROCEDURE UPPER*( VAR str1: ARRAY OF CHAR );
PROCEDURE AsciiChar2Hex*( ch: CHAR ): INTEGER;
PROCEDURE ConvString*( STR: ARRAY OF CHAR ): INTEGER;
PROCEDURE AsciiStr2Number*( STR: ARRAY OF CHAR ): INTEGER;
PROCEDURE HiByte*( D16: LONGINT ): LONGINT;
PROCEDURE LoByte*( D16: LONGINT ): LONGINT;
PROCEDURE GetByte*( VAR R: BinaryRider.Reader; VAR CNT: LONGINT ): CHAR;
PROCEDURE Int2HexChar*( N: LONGINT ): CHAR;
PROCEDURE Int2HexStr*( N: LONGINT; CNT: LONGINT; VAR S: ARRAY OF CHAR );
PROCEDURE GetChar*( VAR R: TextRider.Reader; VAR CNT: LONGINT ): CHAR;
PROCEDURE Is_Printable*( CH: LONGINT ) : CHAR;
PROCEDURE OrdinalOf*( CH: CHAR ) : LONGINT;
PROCEDURE Even*( Num: LONGINT ) : BOOLEAN;
PROCEDURE tBit*( Bit: INTEGER; Num: LONGINT ): INTEGER;
PROCEDURE TBit*( Bit: INTEGER; Num: LONGINT ): BOOLEAN;
PROCEDURE SBit*( Bit: INTEGER; VAR Num: LONGINT ): BOOLEAN;
PROCEDURE RBit*( Bit: INTEGER; VAR Num: LONGINT ): BOOLEAN;
PROCEDURE DumpByte*( Num: LONGINT );
PROCEDURE Spaces*( Cnt: INTEGER );
PROCEDURE Split*( val: LONGINT; VAR hi, lo: LONGINT );
PROCEDURE Unsplit*( VAR val: LONGINT; hi, lo: LONGINT );
PROCEDURE Byte2HexStr*( ch: LONGINT; VAR str: ARRAY OF CHAR );
PROCEDURE Word2HexStr*( wd: LONGINT; VAR str: ARRAY OF CHAR );
PROCEDURE Sprintf*( str: ARRAY OF CHAR; cnt: INTEGER );
PROCEDURE FileExists*( Name: ARRAY OF CHAR ): BOOLEAN;
PROCEDURE SkipWhite*( VAR str: ARRAY OF CHAR );
PROCEDURE GetLine*( VAR File: TextRider.Reader; VAR str: ARRAY OF CHAR; VAR cnt: LONGINT ) : BOOLEAN;
PROCEDURE NewGetChar*( str : ARRAY OF CHAR; VAR idx: LONGINT ) : CHAR;
PROCEDURE IsAlphaNum*( ch : CHAR ) : BOOLEAN;
PROCEDURE NewGetByte*( VAR File: BinaryRider.Reader; VAR CH: CHAR; VAR CNT: LONGINT ): BOOLEAN;
PROCEDURE ExtractWord*( buff: ARRAY OF CHAR; VAR buffIdx: INTEGER; VAR word: ARRAY OF CHAR );
PROCEDURE TrimWhite*( VAR str: ARRAY OF CHAR );
PROCEDURE OpenBinaryReader*( VAR name: ARRAY OF CHAR; VAR f: Files.File; VAR res: Files.Result );
PROCEDURE ShowBits*( Num: LONGINT );
PROCEDURE FileNotOpen*( file: ARRAY OF CHAR );
PROCEDURE FileNotFound*( file: ARRAY OF CHAR );
PROCEDURE XXX
###############################################################*)
IMPORT
	BinaryRider,
	TextRider,
	Files,
	Out,
	CharClass,
	Strings,

	Debug
;

(*###############################################################
# Globals
###############################################################*)
CONST
	File = "Libs/Utils/Utils.Mod";

    Bin =    2;
    Oct =    8;
	Dec =	 10;
    Hex =    16;


VAR
    BASE:    INTEGER;

(*###############################################################
# NAME:		Range
# FUNC:		Determine if a num is between lo and hi inclusive.
# RETURNS:	TRUE if num is between hi and lo inclusively, else
#			FALSE.
# CHANGES:	Nothing.
###############################################################*)
PROCEDURE Range*( num, lo, hi: LONGINT ): BOOLEAN;
BEGIN
    IF ( num >= lo ) & ( num <= hi ) THEN
        RETURN( TRUE );
    ELSE
(*
        Out.String( "ERROR: value '" );
        Out.LongInt( num, 1 );
        Out.String( "' out of range: " );
        Out.LongInt( lo, 1 );
        Out.String( " - " );
        Out.LongInt( hi, 1 );
        Out.Ln;
*)
        RETURN( FALSE );
    END;

END Range;


(*###############################################################
# NAME:        LOWER
# FUNC:        Converts any uppercase CHAR in str1 into an lowercase
#            CHAR.
# RETURNS:    ARRAY OF CHAR.
# CHANGES:    Nothing.
###############################################################*)
PROCEDURE LOWER*( VAR str1: ARRAY OF CHAR );
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;
    len:    LONGINT;
    CH:        CHAR;
    idx:    LONGINT;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    len := Strings.Length( str1 );
    IF DBG THEN
        Debug.Debug;
        Debug.LLInt( "len", len, 2 );
        Debug.Nl;
    END;

    idx := 0;
    WHILE idx < len DO
        CH := str1[ idx ];
        IF DBG THEN
            Debug.Debug;
            Debug.LQChar( "CH", CH );
            Debug.Nl;
        END;
        IF CharClass.IsUpper( CH ) THEN
            str1[ idx ] := CHR( ORD( CH ) + 20H );
        END;
        INC( idx );
    END;
    IF DBG THEN
        Debug.Debug;
        Debug.LQString( "str1", str1 );
        Debug.Nl;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END LOWER;


(*###############################################################
# NAME:        UPPER
# FUNC:        Converts any lowercase CHAR in str1 into an uppercase
#            CHAR.
# RETURNS:    ARRAY OF CHAR.
# CHANGES:    Nothing.
###############################################################*)
PROCEDURE UPPER*( VAR str1: ARRAY OF CHAR );
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;
    len:    LONGINT;
    CH:        CHAR;
    idx:    LONGINT;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    len := Strings.Length( str1 );
    IF DBG THEN
        Debug.Debug;
        Debug.LLInt( "len", len, 2 );
        Debug.Nl;
    END;

    idx := 0;
    WHILE idx < len DO
        CH := str1[ idx ];
        IF DBG THEN
            Debug.Debug;
            Debug.LQChar( "CH", CH );
            Debug.Nl;
        END;
        IF CharClass.IsLower( CH ) THEN
            str1[ idx ] := CHR( ORD( CH ) - 20H );
        END;
        INC( idx );
    END;
    IF DBG THEN
        Debug.Debug;
        Debug.LQString( "str1", str1 );
        Debug.Nl;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END UPPER;


(*###############################################################
# NAME:		AsciiChar2Hex
# FUNC:		Converts an ASCII char into a hex nibble. This
#			does NO checking that the ASCII char is in the hex
#			range (0-9 and A-F).
# RETURNS:	CH.
# CHANGES:	Nothing.
###############################################################*)
PROCEDURE AsciiChar2Hex*( ch: CHAR ): INTEGER;
CONST
	Proc = "LOWER";

VAR
    DBG:	BOOLEAN;
    n:		INTEGER;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    n := ORD( ch ) - 30H;
    IF n > 9H THEN
        n := n - 7H;
    END;

    IF DBG THEN
        Debug.Debug;
        Debug.LLInt( "n", n, 2 );
        Debug.Nl;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( n );
END AsciiChar2Hex;


(*###############################################################
# NAME:        ConvString
# FUNC:        Converts an ASCII string to an INTEGER.
# RETURNS:    CH.
# CHANGES:    Nothing.
###############################################################*)
PROCEDURE ConvString*( STR: ARRAY OF CHAR ): INTEGER;
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;
    NUM:    INTEGER;
    len:    INTEGER;
    idx:    INTEGER;
    val:    INTEGER;
    CH:        CHAR;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Debug;
        Debug.LQString( "STR", STR );
        Debug.Nl;
    END;

    len := Strings.Length( STR );
    DEC( len );
    NUM := 0;
    idx := 0;
    WHILE idx < len DO
        CH := STR[ idx ];
        IF DBG THEN
            Debug.Debug;
            Debug.LQChar( "CH", CH );
            Debug.Nl;
        END;
        IF BASE = Hex THEN
            val := AsciiChar2Hex( CH );
        ELSE
            val := ORD( CH ) - ORD( '0' );
        END;
        IF DBG THEN
            Debug.Debug;
            Debug.LHex( "val", val, 2 );
            Debug.Nl;
        END;
        NUM := NUM * BASE;
        NUM := NUM + val;
        IF DBG THEN
            Debug.Debug;
            Debug.LHex( "NUM", NUM, 4 );
            Debug.Nl;
        END;
        INC( idx );
    END;
    CH := STR[ idx ];
    IF BASE = Hex THEN
        val := AsciiChar2Hex( CH );
    ELSE
        val := ORD( CH ) - ORD( '0' );
    END;
    IF DBG THEN
        Debug.Debug;
        Debug.LHex( "val", val, 2 );
        Debug.Nl;
    END;
    NUM := NUM * BASE;
    NUM := NUM + val;
    IF DBG THEN
        Debug.Debug;
        Debug.LHex( "NUM", NUM, 4 );
        Debug.Nl;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( NUM );
END ConvString;


(*###############################################################
# NAME:		AsciiStr2Number
# FUNC:		Converts an ASCII string a LONGINT.
# RETURNS:	CH.
# CHANGES:	Nothing.
###############################################################*)
PROCEDURE AsciiStr2Number*( STR: ARRAY OF CHAR ): INTEGER;
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;
    NUM:    INTEGER;
    len:    INTEGER;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    (* Capitalize all chars in the string *)
    IF DBG THEN
		Debug.Debug;
        Debug.LQString( "STR before Capilatize is run", STR );
        Debug.Nl;
    END;
    Strings.Capitalize( STR );
    IF DBG THEN
        Debug.Debug;
        Debug.LQString( "STR after Capilatize is run", STR );
        Debug.Nl;
    END;

    len := Strings.Length( STR );
    DEC( len );

	BASE := Dec;
    IF ORD( STR[ len ]) = ORD( "B" ) THEN (* Check for a trailing "B". If found, remove it. *)
        Strings.Delete( STR, len, 1 );
        IF DBG THEN
            Debug.Debug;
            Debug.LQString( "STR after removal of trailing B", STR );
            Debug.Nl;
        END;
        BASE := Bin;
    ELSIF ORD( STR[ len ]) = ORD( "O" ) THEN (* Check for a trailing "O". If found, remove it. *)
        Strings.Delete( STR, len, 1 );
        IF DBG THEN
            Debug.Debug;
            Debug.LQString( "STR after removal of trailing O", STR );
            Debug.Nl;
        END;
        BASE := Oct;
    ELSIF ORD( STR[ len ]) = ORD( "Q" ) THEN (* Check for a trailing Q. If found, remove it. *)
        Strings.Delete( STR, len, 1 );
        IF DBG THEN
            Debug.Debug;
            Debug.LQString( "STR after removal of trailing Q", STR );
            Debug.Nl;
        END;
        BASE := Oct;
    ELSIF ORD( STR[ len ]) = ORD( "H" ) THEN (* Check for a trailing H. If found, remove it. *)
        Strings.Delete( STR, len, 1 );
        IF DBG THEN
            Debug.Debug;
            Debug.LQString( "STR after removal of trailing H", STR );
            Debug.Nl;
        END;
        BASE := Hex;
    ELSIF ORD( STR[ 1 ] ) = ORD( "X" ) THEN (* Check for a leading 0x. If found, remove it. *)
        Strings.Delete( STR, 0, 2 );
        IF DBG THEN
            Debug.Debug;
            Debug.LQString( "STR after delete of 0x was done", STR );
            Debug.Nl;
        END;
        BASE := Hex;
    END;
    NUM := ConvString( STR );

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( NUM );
END AsciiStr2Number;


(*###############################################################
# NAME:        HiByte
# FUNC:        Takes a 16 bit (a word in Z80 land) and returns the
#            high order byte.
# RETURNS:    High order byte of D16 in the lower 8 bits.
# CHANGES:    Nothing.
###############################################################*)
PROCEDURE HiByte*( D16: LONGINT ): LONGINT;
BEGIN
    RETURN( D16 DIV 100H );
END HiByte;


(*###############################################################
# NAME:        LoByte
# FUNC:        Takes a 16 bit (a word in Z80 land) and returns the
#            low order byte.
# RETURNS:    Low order byte of D16.
# CHANGES:    Nothing.
###############################################################*)
PROCEDURE LoByte*( D16: LONGINT ): LONGINT;
BEGIN
    RETURN( D16 MOD 100H );
END LoByte;

(*###############################################################
# NAME:		GetByte
# FUNC:		Reads a byte from R into CH, and increments the
#			counter CNT.
# RETURNS:	CH (CHAR).
# CHANGES:	R and CNT.
###############################################################*)
(* PROCEDURE GetByte*( VAR R: BinaryRider.Reader; VAR CNT: LONGINT ): CHAR; *)
PROCEDURE GetByte*( VAR R: BinaryRider.Reader ): CHAR;
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;

    CH:            CHAR;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
	END;

    R.ReadChar( CH );
    (* INC( CNT ); *)
    IF DBG THEN
        Debug.Debug;
        Debug.LHex( "CH", ORD( CH ), 2 );
(*
        Debug.Spaces( 3 );
		Debug.Label( "CNT" );
        Debug.Hex( CNT, 6 );
        Debug.Nl;
*)
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( CH );
END GetByte;

(*###############################################################
# NAME:        Int2HexChar
# FUNC:        Converts a Hex nibble into an ASCII char.
# RETURNS:    A character.
# CHANGES:    Nothing.
###############################################################*)
PROCEDURE Int2HexChar*( N: LONGINT ): CHAR;
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;
    CH:            CHAR;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF N < 10 THEN
        CH := CHR( N + 30H );
    ELSE
        CH := CHR( N + 37H );
    END;
    IF DBG THEN
        Debug.Debug;
        Debug.LHex( "N", N, 2 );
        Debug.Nl;
        Debug.Debug;
        Debug.LQChar( "CH", CH );
        Debug.Nl;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( CH );
END Int2HexChar;


(*###############################################################
# NAME:        Int2HexStr
# FUNC:        Converts a multi hex nibble number N into a
#            String S and uses CNT number of chacters.
# RETURNS:    S.
# CHANGES:    N and CNT.
###############################################################*)
(*
PROCEDURE Int2HexStr*( N: LONGINT; CNT: LONGINT; VAR S: ARRAY OF CHAR );
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;
    mod:        LONGINT;
    CH:            CHAR;

BEGIN
    DBG := FALSE;
    DBG := TRUE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

        IF DBG THEN
            Debug.Debug( IL );
            Debug.Label( "N" );
            Debug.Hex( N, 2 );
            Debug.Nl;
            Debug.Debug( IL );
            Debug.Label( "CNT" );
            Debug.LInt( CNT, 3 );
            Debug.Nl;
            Debug.Debug( IL );
            Debug.Label( "Str" );
            Debug.QString( S );
            Debug.Nl;
        END;

    S[ 2 ] := 0X;
    WHILE CNT > 0 DO
        mod := N MOD 10H;
        CH := Int2HexChar( mod );
        Strings2.InsertChar( CH, 2, S );
        IF DBG THEN
            Debug.Debug( IL );
            Debug.Label( "mod" );
            Debug.Hex( mod, 2 );
            Debug.Nl;
            Debug.Debug( IL );
            Debug.Label( "CH" );
            Debug.QChar( CH );
            Debug.Nl;
            Debug.Debug( IL );
            Debug.Label( "Str" );
            Debug.QString( S );
            Debug.Nl;
        END;
        N := N DIV 10H;
        DEC( CNT );
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END Int2HexStr;
*)


(*###############################################################
# NAME:        GetChar
# FUNC:        Reads a char from Reader R into CH, and increments
#            the counter CNT.
# RETURNS:    CH.
# CHANGES:    R and CNT.
###############################################################*)
PROCEDURE GetChar*( VAR R: TextRider.Reader; VAR CNT: LONGINT ): CHAR;
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;
    CH:            CHAR;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    R.ReadChar( CH );
    INC( CNT );
    IF DBG THEN
        Debug.Debug;
        Debug.LHex( "CH", ORD( CH ), 2 );
        Debug.Spaces( 3 );
        Debug.LHex( "CNT", CNT, 6 );
        Debug.Nl;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( CH );
END GetChar;


(*###############################################################
# NAME:        Is_Printable
# FUNC:        Determine if character CH is printable.
# RETURNS:    Returns the character if it is, otherwise ".".
# CHANGES:    N/A
###############################################################*)
PROCEDURE Is_Printable*( CH: LONGINT ) : CHAR;
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;

    ch:                CHAR;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
	END;

    IF DBG THEN
		Debug.LHex( "CH", CH, 2 );
	END;

    IF CH < 20H THEN
        ch := ".";
    ELSIF CH > 7EH THEN
        ch := ".";
	ELSE
   		 ch := CHR( CH );
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( ch );
END Is_Printable;


(*###############################################################
# NAME:        OrdinalOf
# FUNC:        Convert CH into Ordinal.
# RETURNS:    Returns the ordinal of CH.
# CHANGES:    N/A
###############################################################*)
PROCEDURE OrdinalOf*( CH: CHAR ) : LONGINT;
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( ORD( CH ));
END OrdinalOf;


(*###############################################################
# NAME:        Even
# FUNC:        Determine if Num is even or odd.
# RETURNS:    Returns TRUE if even, else FALSE.
# CHANGES:    N/A
###############################################################*)
PROCEDURE Even*( Num: LONGINT ) : BOOLEAN;
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    IF Num = 0 THEN
        RETURN( TRUE );
    ELSE
        IF Num MOD 2 = 0 THEN
            RETURN( TRUE );
        ELSE
            RETURN( FALSE );
        END;
    END;
END Even;


(*###############################################################
# NAME:       tBit
# DATE:       20140514
# AUTH:       Fitz - fitzer8@gmail.com
# FUNC:       Tests to determine if bit Bit is 1 or 0.
# RETURNS:    Returns 0 if 0, 1 if 1, else 99.
# CHANGES:    N/A - Rewrite.
# NOTE:       Uses bit numbers: 0,1,2,3,4,5,6,7...
###############################################################*)
PROCEDURE tBit*( Bit: INTEGER; Num: LONGINT ): INTEGER;
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;

    num:    LONGINT;

    Val:    INTEGER;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Debug;
        Debug.Parms;
		Debug.LHex( "Num", Num, 2 );
        Debug.Spaces( 4 );
		Debug.LHex( "Bit Number", Bit, 2 );
        Debug.Nl;
    END;

	Val := 0;
	(* IF(( Bit < 0) OR ( Bit > 7 ))THEN *)
	IF( ~Range( Bit, 0, 7 )) THEN
		Out.String( "Bit Number: '" );
		Out.Int( Bit, 2 );
		Out.String( "' is out of range 0 .. 7" );
		Out.Ln;
		Val := 99;
	END;

	IF( Val = 0 ) THEN
    	(*###############################################################
    	# Bring the bit you want to the 0th bit position.
    	###############################################################*)
    	num := Num;
    	IF Bit > 0 THEN
        Num := ASH( num, -Bit );
    	END;
    
    	IF DBG THEN
        	Debug.Debug;
        	Debug.LHex( "num", num, 2 );
        	Debug.Nl;
        	Debug.Debug;
        	Debug.LHex( "Num", Num, 2 );
        	Debug.Nl;
    	END;
    
    	IF ODD( Num ) THEN
        	Val := 1;
    	ELSE
        	Val := 0;
    	END;
    	IF DBG THEN
        	IF( Val = 1 )THEN
            	Debug.Debug;
            	Debug.Label( "1" );
            	Debug.Nl;
        	ELSE
            	Debug.Debug;
            	Debug.Label( "0" );
            	Debug.Nl;
        	END;
    	END;
	END; (* IF( Val = 0 ) THEN *)

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( Val );
END tBit;

(*###############################################################
# Test bit Bit of Num, return FALSE if not set, else TRUE.
# Uses bit numbers: 0,1,2,3,4,5,6,7...
###############################################################*)
PROCEDURE TBit*( Bit: INTEGER; Num: LONGINT ): BOOLEAN;
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;

    num:    LONGINT;

    Val:    BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Debug;
        Debug.Parms;
        Debug.LHex( "Num", Num, 2 );
        Debug.Spaces( 4 );
        Debug.LHex( "Bit", Bit, 2 );
        Debug.Nl;
    END;

    (*###############################################################
    # Bring the bit you wamt to the 0th bit position.
    ###############################################################*)
    num := Num;
    IF Bit > 0 THEN
        Num := ASH( num, -Bit );
    END;
    
    IF DBG THEN
        Debug.Debug;
        Debug.LHex( "num", num, 2 );
        Debug.Nl;
        Debug.Debug;
        Debug.LHex( "Num", Num, 2 );
        Debug.Nl;
    END;
    
    IF ODD( Num ) THEN
        Val := TRUE;
    ELSE
        Val := FALSE;
    END;
    IF DBG THEN
        IF Val THEN
            Debug.Debug;
            Debug.Label( "TRUE" );
            Debug.Nl;
        ELSE
            Debug.Debug;
            Debug.Label( "FALSE" );
            Debug.Nl;
        END;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( Val );
END TBit;


(*###############################################################
# Set bit Bit of Num, return FALSE if not set, else TRUE.
# Uses bit numbers: 0 .. 7...
PROCEDURE SBit*( Bit: INTEGER; VAR Num: LONGINT ): BOOLEAN;
###############################################################*)
PROCEDURE SBit*( Bit: INTEGER; VAR Num: LONGINT );
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;

    temp:    LONGINT;

    Val:    BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Debug;
        Debug.Parms;
		Debug.Label( "Num" );
        Debug.HexStr( Num, 2 );
        Debug.Spaces( 4 );
        Debug.LHex( "Bit", Bit, 2 );
        Debug.Nl;
    END;

    (*###############################################################
    # Bring the bit you wamt to the Bit bit position.
    ###############################################################*)
    temp := 0;
    IF ~TBit( Bit, Num ) THEN
        IF Bit > 0 THEN
            temp := ASH( 1, Bit );
        END;
        Num := Num + temp;
    END;
    IF DBG THEN
        Debug.Debug;
		Debug.Label( "Num" );
        Debug.HexStr( Num, 2 );
        Debug.Nl;
        Debug.Debug;
		Debug.Label( "temp" );
        Debug.HexStr( temp, 2 );
        Debug.Nl;
    END;
    
    Val := TBit( Bit, Num );
    IF DBG THEN
        IF Val THEN
            Debug.Debug;
            Debug.Label( "TRUE" );
            Debug.Nl;
        ELSE
            Debug.Debug;
            Debug.Label( "FALSE" );
            Debug.Nl;
        END;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    (* RETURN( Val ); *)
END SBit;


(*###############################################################
# Reset bit Bit of Num, return FALSE if not set, else TRUE.
# Uses bit numbers: 0 .. 7...
PROCEDURE RBit*( Bit: INTEGER; VAR Num: LONGINT ): BOOLEAN;
###############################################################*)
PROCEDURE RBit*( Bit: INTEGER; VAR Num: LONGINT );
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;

    temp:    LONGINT;

    Val:    BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Debug;
        Debug.Parms;
		Debug.Label( "Num" );
        Debug.HexStr( Num, 2 );
        Debug.Spaces( 4 );
        Debug.LHex( "Bit", Bit, 2 );
        Debug.Nl;
    END;

    (*###############################################################
    # Bring the bit you wamt to the Bit bit position.
    ###############################################################*)
    temp := 0;
    IF TBit( Bit, Num ) THEN
        IF Bit > 0 THEN
            temp := ASH( 1, Bit );
        END;
        Num := Num - temp;
    END;
    IF DBG THEN
        Debug.Debug;
		Debug.Label( "Num" );
        Debug.HexStr( Num, 2 );
        Debug.Nl;
        Debug.Debug;
		Debug.Label( "temp" );
        Debug.HexStr( temp, 2 );
        Debug.Nl;
    END;
    
    Val := TBit( Bit, Num );
    IF DBG THEN
        IF Val THEN
            Debug.Debug;
            Debug.Label( "TRUE" );
            Debug.Nl;
        ELSE
            Debug.Debug;
            Debug.Label( "FALSE" );
            Debug.Nl;
        END;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    (* RETURN( Val ); *)
END RBit;


(*###############################################################
# NAME:       DumpByte
# DATE:       20140514
# AUTH:       Fitz - fitzer8@gmail.com
# FUNC:       Prints to the screen the bits of a CHAR (byte).
# RETURNS:    N/A - Writes to screen.
# CHANGES:    N/A - Rewrite.
###############################################################*)
PROCEDURE DumpByte*( Num: LONGINT );
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;

    num:    LONGINT;

    idx:    INTEGER;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Debug;
        Debug.Parms;
        Debug.LHex( "Num", Num, 2 );
        Debug.Nl;
    END;

	FOR idx := 7 TO 0 BY -1 DO
		(* IF( Bits.tBit( idx, Num ) = 0 ) THEN *)
		IF( TBit( idx, Num )) THEN
			Out.Char( "0" );
		ELSE
			Out.Char( "1" );
		END;
	END;
	Out.Ln;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END DumpByte;

(*###############################################################
# Display Cnt spaces.
###############################################################*)
PROCEDURE Spaces*( Cnt: INTEGER );
CONST
	Proc = "LOWER";

VAR
    DBG:    BOOLEAN;

    Idx:    INTEGER;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    Idx := 0;
    WHILE Idx < Cnt DO
        Out.Char( " " );
        INC( Idx );
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END Spaces;

(*###############################################################
# Title:	Split
# Func:		Split a WORD into two BYTES.
# Author:	Fitz
# Ver:		0.0
# Chg:		hi and lo.
# Rtns:		N/A
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# Unknown  # Fitz       # Original.
# 20100927 # Fitz       # Moved to here, add header and cleanup.
###############################################################*)
PROCEDURE Split*( val: LONGINT; VAR hi, lo: LONGINT );
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;
    
    lo := val MOD 100H;
    hi := val DIV 100H;
    IF DBG THEN
        Debug.LHex( "hi", hi, 3 );
        Debug.Spaces( 2 );
        Debug.LHex( "lo", lo, 3 );
        Debug.Nl;
    END;
    
    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END Split;

(*###############################################################
# Title:	Unsplit
# Func:		Combine the two BYTEs into a WORD.
# Author:	Fitz
# Ver:		0.0
# Chg:		val.
# Rtns:		N/A
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# Unknown  # Fitz       # Original.
# 20100927 # Fitz       # Moved to here, add header and cleanup.
###############################################################*)
PROCEDURE Unsplit*( VAR val: LONGINT; hi, lo: LONGINT );
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;
    
		val := hi * 100H + lo;
    IF DBG THEN
        Debug.LHex( "val", val, 4 );
        Debug.Nl;
    END;
    
    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END Unsplit;

(*###############################################################
# NAME:		Byte2HexStr
# FUNC:		Converts 1 byte "ch" into ASCII and put into str.
# RETURNS:	str
# CHANGES:	str
###############################################################*)
PROCEDURE Byte2HexStr*( ch: LONGINT; VAR str: ARRAY OF CHAR );
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;
    hi, lo:        LONGINT;
    Chi, Clo:    CHAR;
    HexNibs:    ARRAY 17 OF CHAR;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    Strings.Assign( "0123456789ABCDEF", HexNibs );

    hi := ch DIV 16;
    lo := ch MOD 16;

    Chi := HexNibs[ hi ];
    Clo := HexNibs[ lo ];
    str[ 0 ] := Chi;
    str[ 1 ] := Clo;
    str[ 2 ] := 0X;
    IF DBG THEN
        Debug.Debug;
        Debug.LQString( "HexNibs", HexNibs );
        Debug.Nl;
        Debug.Debug;
        Debug.LHex( "ch", ch, 2 );
        Debug.Nl;
        Debug.Debug;
        Debug.LHex( "hi", hi, 2 );
        Debug.Nl;
        Debug.Debug;
        Debug.LHex( "lo", lo, 2 );
        Debug.Nl;
        Debug.Debug;
        Debug.LQString( "str", str );
        Debug.Nl;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END Byte2HexStr;

(*###############################################################
# NAME:        Word2HexStr
# FUNC:        Converts a word (16 bits) into a string.
# RETURNS:    str
# CHANGES:    str
###############################################################*)
PROCEDURE Word2HexStr*( wd: LONGINT; VAR str: ARRAY OF CHAR );
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;
	bHi, bLo:        LONGINT;
    hi, lo:        LONGINT;
    Chi, Clo:    CHAR;
    HexNibs:    ARRAY 17 OF CHAR;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
	
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    Strings.Assign( "0123456789ABCDEF", HexNibs );
	Split( wd, bHi, bLo );

    hi := bHi DIV 16;
    lo := bHi MOD 16;

    Chi := HexNibs[ hi ];
    Clo := HexNibs[ lo ];
	
    str[ 0 ] := Chi;
    str[ 1 ] := Clo;
    
	hi := bLo DIV 16;
    lo := bLo MOD 16;

    Chi := HexNibs[ hi ];
    Clo := HexNibs[ lo ];
	
	str[ 2 ] := Chi;
    str[ 3 ] := Clo;
    str[ 4 ] := 0X;
    IF DBG THEN
        Debug.Debug;
        Debug.LQString( "HexNibs", HexNibs );
        Debug.Nl;
        Debug.Debug;
        Debug.LHex( "wd", wd, 4 );
        Debug.Nl;
        Debug.Debug;
        Debug.LHex( "hi", hi, 2 );
        Debug.Nl;
        Debug.Debug;
        Debug.LHex( "lo", lo, 2 );
        Debug.Nl;
        Debug.Debug;
        Debug.LQString( "str", str );
        Debug.Nl;
    END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END Word2HexStr;


(*###############################################################
# Print str left justified in a field cnt long.
###############################################################*)
PROCEDURE Sprintf*( str: ARRAY OF CHAR; cnt: INTEGER );
CONST
	Proc = "LOWER";

VAR
    DBG:        BOOLEAN;

    len, diff:    INTEGER;
    tmp:        INTEGER;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;
    
    diff := 0;
    len := Strings.Length( str );
    IF len > cnt THEN
        len := cnt;
    END;
    IF len < cnt THEN
        diff := cnt - len;
    END;
    IF DBG THEN
        Debug.LInt( "cnt", cnt, 3 );
        Debug.Spaces( 2 );
        Debug.LInt( "len", len, 3 );
        Debug.Spaces( 2 );
        Debug.LInt( "diff", diff, 3 );
        Debug.Nl;
        Debug.Char( "'" );
    END;
    tmp := 0;
    WHILE len > 0 DO
        Out.Char( str[ tmp ]);
        INC( tmp );
        DEC( len );
    END;
    WHILE diff > 0 DO
        Out.Char( " " );
        DEC( diff );
    END;
    
    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END Sprintf;

(*###############################################################
# Find out if FileName exists and return TRUE if it does else
# FALSE.
###############################################################*)
PROCEDURE FileExists*( Name: ARRAY OF CHAR ): BOOLEAN;
CONST
	Proc = "LOWER";

VAR
    found :		BOOLEAN;

    DBG :		BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;
    
    IF Files.Exists( Name ) THEN
        IF DBG THEN
            Debug.Debug;
            Debug.String( "Found File: " );
            Debug.String( Name );
            Debug.Nl;
        END;
        found := TRUE;
    ELSE
        IF DBG THEN
            Debug.Debug;
            Debug.String( "ERROR:  File: " );
            Debug.String( Name );
            Debug.String( " not found!" );
            Debug.Nl;
        END;
        found := FALSE;
    END;
    
    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN found;
END FileExists;

(*###############################################################
# NAME:		SkipWhite
# FUNC:		Skip (delete) white space from str.
# RETURNS:	Returns str.
# CHANGES:	str
###############################################################*)
PROCEDURE SkipWhite*( VAR str: ARRAY OF CHAR );
CONST
	Proc = "LOWER";

VAR
	DBG		: BOOLEAN;

	idx		: INTEGER;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
        Debug.Entry( File, Proc );
	END;

	idx := 0;
	WHILE( CharClass.IsWhiteSpace( str[ idx ]) & ( str[ idx ] # 0X )) DO
		INC( idx );
	END;
	Strings.Delete( str, 0, idx );

	IF DBG THEN
        Debug.Exit( File, Proc );
	END;
END SkipWhite;

(*###############################################################
# Title:	GetLine
# Func:		Get a line from input File.
# Author:	Fitz
# Ver:		0.0
# Chg:		File, str and cnt.
# Rtns:		TRUE if it got a line else FALSE.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100310 # Fitz       # Original.
###############################################################*)
PROCEDURE GetLine*( VAR inp: TextRider.Reader; VAR str: ARRAY OF CHAR; VAR cnt: LONGINT ) : BOOLEAN;
CONST
	Proc = "LOWER";

VAR
	DBG:		BOOLEAN;

	Done:		BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	
	IF DBG THEN
Debug.Entry( File, Proc );
	END;

	Done := TRUE;
	inp.ReadLine( str );
	IF DBG THEN
		Debug.Debug;
		Debug.LLInt( "cnt", cnt, 4 );
		Debug.Spaces( 4 );
		Debug.LQString( "str", str );
		Debug.Nl;
	END;

	IF inp.res # Files.done THEN
		Done := FALSE;
	ELSE
		INC( cnt );
	END;

	IF DBG THEN
        Debug.Exit( File, Proc );
	END;
	RETURN Done;
END GetLine;

(*###############################################################
# Title:	ProcessArgs
# Func:		Process the program Arguments.
# Author:	Fitz
# Ver:		0.0
# Chg:		TextRider, str and CNT.
# Rtns:		TRUE if it got a line else FALSE.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100206 # Fitz       # Original.
###############################################################*)
PROCEDURE NewGetChar*( str : ARRAY OF CHAR; VAR idx: LONGINT ) : CHAR;
CONST
	Proc = "LOWER";

VAR
	DBG					: BOOLEAN;

	ch					: CHAR;

BEGIN
	DBG := FALSE;
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
        Debug.Entry( File, Proc );
	END;

	ch := str[ idx ];
	INC( idx );

	IF DBG THEN
		Debug.Debug;
		Debug.LQString( "str", str );
		Debug.Spaces( 4 );
		Debug.LQChar( "ch", ch );
		Debug.Spaces( 4 );
		Debug.LLInt( "idx", idx, 4 );
		Debug.Nl;
	END;
	IF DBG THEN
        Debug.Exit( File, Proc );
	END;
	RETURN ch;
END NewGetChar;
(*
*)

(*###############################################################
# Title:	IsAlphaNum
# Func:		Determines if char is an alpha or number.
# Author:	Fitz
# Ver:		0.0
# Chg:		Nothing
# Rtns:		TRUE if it alpha or number.
#================================================================
# HISTORY
#================================================================
#   DATE   #    NAME    # COMMENT
#================================================================
# 20100311 # Fitz       # Original.
###############################################################*)
PROCEDURE IsAlphaNum*( ch : CHAR ) : BOOLEAN;
CONST
	Proc = "LOWER";

VAR
	res:	BOOLEAN;

	DBG:	BOOLEAN;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
        Debug.Entry( File, Proc );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.LQChar( "ch", ch );
		Debug.Nl;
	END;

	res := FALSE;
	IF(( CharClass.IsNumeric( ch )) OR ( CharClass.IsLetter( ch ))) THEN
		res := TRUE;
	END;

	IF DBG THEN
        Debug.Exit( File, Proc );
	END;
	RETURN res;
END IsAlphaNum;

(*###############################################################
# NAME:		NewGetByte
# FUNC:		Reads a byte from File into ch, and increments the
#			counter cnt.
# USAGE:	WHILE( Buffer.NewGetByte( Glbl.rCOM, ch, cnt )) DO
# RETURNS:	eof - End Of File.
# CHANGES:	File, ch and cnt.
###############################################################*)
PROCEDURE NewGetByte*( VAR inp: BinaryRider.Reader; VAR CH: CHAR; VAR CNT: LONGINT ): BOOLEAN;
CONST
	Proc = "LOWER";

VAR
    DBG:	BOOLEAN;

	eof:	BOOLEAN;
	
BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
	END;

	eof := TRUE;
    inp.ReadChar( CH );
    IF DBG THEN
        Debug.Debug;
        Debug.LHex( "CH", ORD( CH ), 2 );
        Debug.Spaces( 2 );
        Debug.LHex( "CNT", CNT, 6 );
        Debug.Nl;
    END;

	IF inp.res # Files.done THEN
		eof := FALSE;
	ELSE
		INC( CNT );
	END;
	
    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
    RETURN( eof );
	
END NewGetByte;

(*###############################################################
# NAME:		ExtractWord
# FUNC:		Reads a byte from File into ch, and increments the
#			counter cnt.
# USAGE:	WHILE( Buffer.NewGetByte( Glbl.rCOM, ch, cnt )) DO
# RETURNS:	eof - End Of File.
# CHANGES:	File, ch and cnt.
###############################################################*)
PROCEDURE ExtractWord*( buff: ARRAY OF CHAR; VAR buffIdx: INTEGER; VAR word: ARRAY OF CHAR );
CONST
	Proc = "ExtractWord";

VAR
    DBG:	BOOLEAN;

	idx:	INTEGER;
	
BEGIN
    DBG := TRUE;
    DBG := FALSE;


    IF DBG THEN
        Debug.Entry( File, Proc );
	END;

	IF CharClass.IsWhiteSpace( buff[ buffIdx ]) THEN
		IF DBG THEN
			Debug.Debug;
			Debug.String( "Skipping White Space" );
			Debug.Nl;
		END;
		(* WHILE( CharClass.IsWhiteSpace( buff[ buffIdx ])) OR ( buff[ buffIdx ] # 0X ) DO *)
		LOOP
			IF( ~CharClass.IsWhiteSpace( buff[ buffIdx ])) THEN EXIT; END;
			IF( buff[ buffIdx ] = 0X ) THEN EXIT; END;
			INC( buffIdx );
		END;
	END;

	idx := 0;
	LOOP
		IF( CharClass.IsWhiteSpace( buff[ buffIdx ])) THEN EXIT; END;
		IF( buff[ buffIdx ] = 0X ) THEN EXIT; END;
		word[ idx ] := buff[ buffIdx ];
		INC( buffIdx );
		INC( idx );
		word[ idx ] := 0X;
		IF DBG THEN
			Debug.Debug;
			Debug.LQString( "word", word );
			Debug.Nl;
		END;
	END;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END ExtractWord;

(*###############################################################
# NAME:		TrimWhite
# FUNC:		Trim (delete) white space from end of str.
# RETURNS:	Returns str.
# CHANGES:	str
###############################################################*)
PROCEDURE TrimWhite*( VAR str: ARRAY OF CHAR );
CONST
	Proc = "LOWER";

VAR
	DBG		: BOOLEAN;

	idx		: INTEGER;
	len		: INTEGER;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
        Debug.Entry( File, Proc );
	END;

	idx := 0;
	len := Strings.Length( str ) - 1;
	IF DBG THEN
		Debug.Debug;
		Debug.LQString( "str", str );
		Debug.Nl;
		Debug.Debug;
		Debug.LInt( "len", len, 2 );
		Debug.Nl;
		Debug.Debug;
		Debug.LQChar( "char", str[len] );
		Debug.Nl;
	END;

	IF str[ len ] = 0X THEN
		DEC( len );
	END;
	WHILE CharClass.IsWhiteSpace( str[ len ]) DO
		str[ len ] := 0X;
		IF DBG THEN
			Debug.Debug;
			Debug.LInt( "len", len, 2 );
			Debug.Nl;
			Debug.Debug;
			Debug.LQString( "str", str );
			Debug.Nl;
		END;
		DEC( len );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.LQString( "str", str );
		Debug.Nl;
	END;
	IF DBG THEN
        Debug.Exit( File, Proc );
	END;
END TrimWhite;

(*###############################################################
# NAME:		OpenBinaryReader
# FUNC:		Trim (delete) white space from end of str.
# RETURNS:	Returns str.
# CHANGES:	str
###############################################################*)
PROCEDURE OpenBinaryReader*( VAR name: ARRAY OF CHAR; VAR f: Files.File; VAR res: Files.Result );
CONST
	Proc = "LOWER";

VAR
	DBG		: BOOLEAN;

	idx:	INTEGER;
	len:	INTEGER;

BEGIN
	DBG := TRUE;
	DBG := FALSE;
	IF DBG THEN
        Debug.Entry( File, Proc );
	END;

	IF DBG THEN
		Debug.Debug;
		Debug.LQString( "name", name );
		Debug.Nl;
	END;

	IF ~Files.Exists( name ) THEN
		Out.String( "ERROR:  File: " );
		Out.String( name );
		Out.String( " not found!" );
		Out.Ln;
		HALT( 1 );
	END;

	f := Files.Old( name, {Files.read}, res );

	IF DBG THEN
        Debug.Exit( File, Proc );
	END;
END OpenBinaryReader;


(*###############################################################
# NAME:       ShowBits
# DATE:       20160916
# AUTH:       Fitz - fitzer8@gmail.com
# FUNC:       Prints to the screen the bits of a WORD (32 bits).
# RETURNS:    N/A - Writes to screen.
# CHANGES:    N/A - Rewrite.
###############################################################*)
PROCEDURE ShowBits*( Num: LONGINT );
CONST
	Proc = "ShowBits";

VAR
    DBG:    BOOLEAN;

    num:    LONGINT;

    idx:    INTEGER;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;

    IF DBG THEN
        Debug.Debug;
        Debug.Parms;
        Debug.Label( "Num" );
        Debug.Hex( Num, 2 );
        Debug.Nl;
    END;

	FOR idx := 32 TO 0 BY -1 DO
		(* IF( Bits.TBit( idx, Num ) = 0 ) THEN *)
		IF( TBit( idx, Num )) THEN
			Out.Char( "0" );
		ELSE
			Out.Char( "1" );
		END;
	END;
	Out.Ln;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END ShowBits;


(*###############################################################
# NAME:       FileNotOpen
# DATE:       20160918
# AUTH:       Fitz - fitzer8@gmail.com
# FUNC:       Prints to the screen FileNotOpen message.
# RETURNS:    N/A - Writes to screen.
# CHANGES:    N/A - Rewrite.
###############################################################*)
PROCEDURE FileNotOpen*( file: ARRAY OF CHAR );
CONST
	Proc = "FileNotOpen";

VAR
    DBG:    BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;


	Debug.LQString( "ERROR:  Can't open file", file );
	Out.Ln;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END FileNotOpen;


(*###############################################################
# NAME:       FileNotFound
# DATE:       20160918
# AUTH:       Fitz - fitzer8@gmail.com
# FUNC:       Prints to the screen FileNotFound message.
# RETURNS:    N/A - Writes to screen.
# CHANGES:    N/A - Rewrite.
###############################################################*)
PROCEDURE FileNotFound*( file: ARRAY OF CHAR );
CONST
	Proc = "FileNotFound";

VAR
    DBG:    BOOLEAN;

BEGIN
    DBG := TRUE;
    DBG := FALSE;
    IF DBG THEN
        Debug.Entry( File, Proc );
    END;


	Debug.LQString( "ERROR:  Can't find file", file );
	Out.Ln;

    IF DBG THEN
        Debug.Exit( File, Proc );
    END;
END FileNotFound;


BEGIN
END Utils.

(*###############################################################
FitzFitz
###############################################################*)



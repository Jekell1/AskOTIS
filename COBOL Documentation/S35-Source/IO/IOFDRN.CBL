       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOFDRN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-10-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOFDRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : FDFILE 
      * TABLE DESC    : FORM DESCRIPTOR FILE
      * OUTPUT FILE   : XPC/IOFDRN.PRE
      * USED FOR      : LIBLP/LPWSFD.CPY  --- QFD-REC
      *                 LIBLP/LPFD1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0310 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOFDRN.CBL S34 05/18/21-07:45:04 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LPWSFD.CPY".

      *#################################################################
      *   BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBLP/LPWSFD.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QFD1-WHERE-BEG-KEY.
           03  QFD1-WBEG-FMNO           PIC 9(04).

       01  QFD1-WHERE-END-KEY.
           03  QFD1-WEND-FMNO           PIC 9(04).

       01  QFD-REC.
           03  QFD-FMNO                     PIC 9(4).
           03  QFD-LTOUCH-DATE              PIC X(10).
           03  QFD-DESC                     PIC X(25).
           03  QFD-OPTION                   PIC X(1).
           03  QFD-FORM-CHG-CD              PIC X.
           03  QFD-PROG-PREFIX              PIC X.
           03  QFD-TYPE                     PIC X.
           03  QFD-LNMEMO               PIC X.
           03  QFD-BODY                 PIC 9(4).
           03  QFD-TOP                  PIC 9(4).
           03  QFD-BOTTOM               PIC 9(4).
           03  QFD-PROG                 PIC XX.
           03  QFD-PROMPTS-1            PIC X(80).
           03  QFD-PROMPTS-2            PIC X(80).
           03  QFD-PROMPTS-3            PIC X(80).
           03  QFD-PROMPTS-4            PIC X(80).
           03  QFD-PROMPTS-5            PIC X(80).
           03  QFD-PROMPTS-6            PIC X(80).
           03  QFD-PROMPTS-7            PIC X(80).
           03  QFD-PROMPTS-8            PIC X(80).
           03  QFD-PROMPTS-9            PIC X(80).
           03  QFD-PROMPTS-10           PIC X(80).
           03  QFD-PROMPTS-11           PIC X(80).
           03  QFD-PROMPTS-12           PIC X(80).
           03  QFD-PROMPTS-13           PIC X(80).
           03  QFD-PROMPTS-14           PIC X(80).
           03  QFD-PROMPTS-15           PIC X(80).
           03  QFD-PROMPTS-16           PIC X(80).
           03  QFD-PROMPTS-17           PIC X(80).
           03  QFD-PROMPTS-18           PIC X(80).
           03  QFD-PROMPTS-19           PIC X(80).
           03  QFD-PROMPTS-20           PIC X(80).
           03  QFD-PROMPTS-21           PIC X(80).
           03  QFD-PROMPTS-22           PIC X(80).
           03  QFD-PROMPTS-23           PIC X(80).
           03  QFD-PROMPTS-24           PIC X(80).
           03  QFD-PROMPTS-25           PIC X(80).
           03  QFD-PROMPTS-26           PIC X(80).
           03  QFD-PROMPTS-27           PIC X(80).
           03  QFD-PROMPTS-28           PIC X(80).
           03  QFD-PROMPTS-29           PIC X(80).
           03  QFD-PROMPTS-30           PIC X(80).
           03  QFD-FIELDNO-1      PIC 9(4).
           03  QFD-FIELDROW-1     PIC 9(2).
           03  QFD-FIELDCOL-1     PIC 9(2).
           03  QFD-FIELDED-1      PIC 9(2).
           03  QFD-FIELDKEY-1     PIC 9(4).
           03  QFD-FIELDNO-2      PIC 9(4).
           03  QFD-FIELDROW-2     PIC 9(2).
           03  QFD-FIELDCOL-2     PIC 9(2).
           03  QFD-FIELDED-2      PIC 9(2).
           03  QFD-FIELDKEY-2     PIC 9(4).
           03  QFD-FIELDNO-3      PIC 9(4).
           03  QFD-FIELDROW-3     PIC 9(2).
           03  QFD-FIELDCOL-3     PIC 9(2).
           03  QFD-FIELDED-3      PIC 9(2).
           03  QFD-FIELDKEY-3     PIC 9(4).
           03  QFD-FIELDNO-4      PIC 9(4).
           03  QFD-FIELDROW-4     PIC 9(2).
           03  QFD-FIELDCOL-4     PIC 9(2).
           03  QFD-FIELDED-4      PIC 9(2).
           03  QFD-FIELDKEY-4     PIC 9(4).
           03  QFD-FIELDNO-5      PIC 9(4).
           03  QFD-FIELDROW-5     PIC 9(2).
           03  QFD-FIELDCOL-5     PIC 9(2).
           03  QFD-FIELDED-5      PIC 9(2).
           03  QFD-FIELDKEY-5     PIC 9(4).
           03  QFD-FIELDNO-6      PIC 9(4).
           03  QFD-FIELDROW-6     PIC 9(2).
           03  QFD-FIELDCOL-6     PIC 9(2).
           03  QFD-FIELDED-6      PIC 9(2).
           03  QFD-FIELDKEY-6     PIC 9(4).
           03  QFD-FIELDNO-7      PIC 9(4).
           03  QFD-FIELDROW-7     PIC 9(2).
           03  QFD-FIELDCOL-7     PIC 9(2).
           03  QFD-FIELDED-7      PIC 9(2).
           03  QFD-FIELDKEY-7     PIC 9(4).
           03  QFD-FIELDNO-8      PIC 9(4).
           03  QFD-FIELDROW-8     PIC 9(2).
           03  QFD-FIELDCOL-8     PIC 9(2).
           03  QFD-FIELDED-8      PIC 9(2).
           03  QFD-FIELDKEY-8     PIC 9(4).
           03  QFD-FIELDNO-9      PIC 9(4).
           03  QFD-FIELDROW-9     PIC 9(2).
           03  QFD-FIELDCOL-9     PIC 9(2).
           03  QFD-FIELDED-9      PIC 9(2).
           03  QFD-FIELDKEY-9     PIC 9(4).
           03  QFD-FIELDNO-10     PIC 9(4).
           03  QFD-FIELDROW-10    PIC 9(2).
           03  QFD-FIELDCOL-10    PIC 9(2).
           03  QFD-FIELDED-10     PIC 9(2).
           03  QFD-FIELDKEY-10    PIC 9(4).
           03  QFD-FIELDNO-11     PIC 9(4).
           03  QFD-FIELDROW-11    PIC 9(2).
           03  QFD-FIELDCOL-11    PIC 9(2).
           03  QFD-FIELDED-11     PIC 9(2).
           03  QFD-FIELDKEY-11    PIC 9(4).
           03  QFD-FIELDNO-12     PIC 9(4).
           03  QFD-FIELDROW-12    PIC 9(2).
           03  QFD-FIELDCOL-12    PIC 9(2).
           03  QFD-FIELDED-12     PIC 9(2).
           03  QFD-FIELDKEY-12    PIC 9(4).
           03  QFD-FIELDNO-13     PIC 9(4).
           03  QFD-FIELDROW-13    PIC 9(2).
           03  QFD-FIELDCOL-13    PIC 9(2).
           03  QFD-FIELDED-13     PIC 9(2).
           03  QFD-FIELDKEY-13    PIC 9(4).
           03  QFD-FIELDNO-14     PIC 9(4).
           03  QFD-FIELDROW-14    PIC 9(2).
           03  QFD-FIELDCOL-14    PIC 9(2).
           03  QFD-FIELDED-14     PIC 9(2).
           03  QFD-FIELDKEY-14    PIC 9(4).
           03  QFD-FIELDNO-15     PIC 9(4).
           03  QFD-FIELDROW-15    PIC 9(2).
           03  QFD-FIELDCOL-15    PIC 9(2).
           03  QFD-FIELDED-15     PIC 9(2).
           03  QFD-FIELDKEY-15    PIC 9(4).
           03  QFD-FIELDNO-16     PIC 9(4).
           03  QFD-FIELDROW-16    PIC 9(2).
           03  QFD-FIELDCOL-16    PIC 9(2).
           03  QFD-FIELDED-16     PIC 9(2).
           03  QFD-FIELDKEY-16    PIC 9(4).
           03  QFD-FIELDNO-17     PIC 9(4).
           03  QFD-FIELDROW-17    PIC 9(2).
           03  QFD-FIELDCOL-17    PIC 9(2).
           03  QFD-FIELDED-17     PIC 9(2).
           03  QFD-FIELDKEY-17    PIC 9(4).
           03  QFD-FIELDNO-18     PIC 9(4).
           03  QFD-FIELDROW-18    PIC 9(2).
           03  QFD-FIELDCOL-18    PIC 9(2).
           03  QFD-FIELDED-18     PIC 9(2).
           03  QFD-FIELDKEY-18    PIC 9(4).
           03  QFD-FIELDNO-19     PIC 9(4).
           03  QFD-FIELDROW-19    PIC 9(2).
           03  QFD-FIELDCOL-19    PIC 9(2).
           03  QFD-FIELDED-19     PIC 9(2).
           03  QFD-FIELDKEY-19    PIC 9(4).
           03  QFD-FIELDNO-20     PIC 9(4).
           03  QFD-FIELDROW-20    PIC 9(2).
           03  QFD-FIELDCOL-20    PIC 9(2).
           03  QFD-FIELDED-20     PIC 9(2).
           03  QFD-FIELDKEY-20    PIC 9(4).
           03  QFD-FIELDNO-21     PIC 9(4).
           03  QFD-FIELDROW-21    PIC 9(2).
           03  QFD-FIELDCOL-21    PIC 9(2).
           03  QFD-FIELDED-21     PIC 9(2).
           03  QFD-FIELDKEY-21    PIC 9(4).
           03  QFD-FIELDNO-22     PIC 9(4).
           03  QFD-FIELDROW-22    PIC 9(2).
           03  QFD-FIELDCOL-22    PIC 9(2).
           03  QFD-FIELDED-22     PIC 9(2).
           03  QFD-FIELDKEY-22    PIC 9(4).
           03  QFD-FIELDNO-23     PIC 9(4).
           03  QFD-FIELDROW-23    PIC 9(2).
           03  QFD-FIELDCOL-23    PIC 9(2).
           03  QFD-FIELDED-23     PIC 9(2).
           03  QFD-FIELDKEY-23    PIC 9(4).
           03  QFD-FIELDNO-24     PIC 9(4).
           03  QFD-FIELDROW-24    PIC 9(2).
           03  QFD-FIELDCOL-24    PIC 9(2).
           03  QFD-FIELDED-24     PIC 9(2).
           03  QFD-FIELDKEY-24    PIC 9(4).
           03  QFD-FIELDNO-25     PIC 9(4).
           03  QFD-FIELDROW-25    PIC 9(2).
           03  QFD-FIELDCOL-25    PIC 9(2).
           03  QFD-FIELDED-25     PIC 9(2).
           03  QFD-FIELDKEY-25    PIC 9(4).
           03  QFD-FIELDNO-26     PIC 9(4).
           03  QFD-FIELDROW-26    PIC 9(2).
           03  QFD-FIELDCOL-26    PIC 9(2).
           03  QFD-FIELDED-26     PIC 9(2).
           03  QFD-FIELDKEY-26    PIC 9(4).
           03  QFD-FIELDNO-27     PIC 9(4).
           03  QFD-FIELDROW-27    PIC 9(2).
           03  QFD-FIELDCOL-27    PIC 9(2).
           03  QFD-FIELDED-27     PIC 9(2).
           03  QFD-FIELDKEY-27    PIC 9(4).
           03  QFD-FIELDNO-28     PIC 9(4).
           03  QFD-FIELDROW-28    PIC 9(2).
           03  QFD-FIELDCOL-28    PIC 9(2).
           03  QFD-FIELDED-28     PIC 9(2).
           03  QFD-FIELDKEY-28    PIC 9(4).
           03  QFD-FIELDNO-29     PIC 9(4).
           03  QFD-FIELDROW-29    PIC 9(2).
           03  QFD-FIELDCOL-29    PIC 9(2).
           03  QFD-FIELDED-29     PIC 9(2).
           03  QFD-FIELDKEY-29    PIC 9(4).
           03  QFD-FIELDNO-30     PIC 9(4).
           03  QFD-FIELDROW-30    PIC 9(2).
           03  QFD-FIELDCOL-30    PIC 9(2).
           03  QFD-FIELDED-30     PIC 9(2).
           03  QFD-FIELDKEY-30    PIC 9(4).
           03  QFD-FIELDNO-31     PIC 9(4).
           03  QFD-FIELDROW-31    PIC 9(2).
           03  QFD-FIELDCOL-31    PIC 9(2).
           03  QFD-FIELDED-31     PIC 9(2).
           03  QFD-FIELDKEY-31    PIC 9(4).
           03  QFD-FIELDNO-32     PIC 9(4).
           03  QFD-FIELDROW-32    PIC 9(2).
           03  QFD-FIELDCOL-32    PIC 9(2).
           03  QFD-FIELDED-32     PIC 9(2).
           03  QFD-FIELDKEY-32    PIC 9(4).
           03  QFD-FIELDNO-33     PIC 9(4).
           03  QFD-FIELDROW-33    PIC 9(2).
           03  QFD-FIELDCOL-33    PIC 9(2).
           03  QFD-FIELDED-33     PIC 9(2).
           03  QFD-FIELDKEY-33    PIC 9(4).
           03  QFD-FIELDNO-34     PIC 9(4).
           03  QFD-FIELDROW-34    PIC 9(2).
           03  QFD-FIELDCOL-34    PIC 9(2).
           03  QFD-FIELDED-34     PIC 9(2).
           03  QFD-FIELDKEY-34    PIC 9(4).
           03  QFD-FIELDNO-35     PIC 9(4).
           03  QFD-FIELDROW-35    PIC 9(2).
           03  QFD-FIELDCOL-35    PIC 9(2).
           03  QFD-FIELDED-35     PIC 9(2).
           03  QFD-FIELDKEY-35    PIC 9(4).
           03  QFD-FIELDNO-36     PIC 9(4).
           03  QFD-FIELDROW-36    PIC 9(2).
           03  QFD-FIELDCOL-36    PIC 9(2).
           03  QFD-FIELDED-36     PIC 9(2).
           03  QFD-FIELDKEY-36    PIC 9(4).
           03  QFD-FIELDNO-37     PIC 9(4).
           03  QFD-FIELDROW-37    PIC 9(2).
           03  QFD-FIELDCOL-37    PIC 9(2).
           03  QFD-FIELDED-37     PIC 9(2).
           03  QFD-FIELDKEY-37    PIC 9(4).
           03  QFD-FIELDNO-38     PIC 9(4).
           03  QFD-FIELDROW-38    PIC 9(2).
           03  QFD-FIELDCOL-38    PIC 9(2).
           03  QFD-FIELDED-38     PIC 9(2).
           03  QFD-FIELDKEY-38    PIC 9(4).
           03  QFD-FIELDNO-39     PIC 9(4).
           03  QFD-FIELDROW-39    PIC 9(2).
           03  QFD-FIELDCOL-39    PIC 9(2).
           03  QFD-FIELDED-39     PIC 9(2).
           03  QFD-FIELDKEY-39    PIC 9(4).
           03  QFD-FIELDNO-40     PIC 9(4).
           03  QFD-FIELDROW-40    PIC 9(2).
           03  QFD-FIELDCOL-40    PIC 9(2).
           03  QFD-FIELDED-40     PIC 9(2).
           03  QFD-FIELDKEY-40    PIC 9(4).
           03  QFD-FIELDNO-41     PIC 9(4).
           03  QFD-FIELDROW-41    PIC 9(2).
           03  QFD-FIELDCOL-41    PIC 9(2).
           03  QFD-FIELDED-41     PIC 9(2).
           03  QFD-FIELDKEY-41    PIC 9(4).
           03  QFD-FIELDNO-42     PIC 9(4).
           03  QFD-FIELDROW-42    PIC 9(2).
           03  QFD-FIELDCOL-42    PIC 9(2).
           03  QFD-FIELDED-42     PIC 9(2).
           03  QFD-FIELDKEY-42    PIC 9(4).
           03  QFD-FIELDNO-43     PIC 9(4).
           03  QFD-FIELDROW-43    PIC 9(2).
           03  QFD-FIELDCOL-43    PIC 9(2).
           03  QFD-FIELDED-43     PIC 9(2).
           03  QFD-FIELDKEY-43    PIC 9(4).
           03  QFD-FIELDNO-44     PIC 9(4).
           03  QFD-FIELDROW-44    PIC 9(2).
           03  QFD-FIELDCOL-44    PIC 9(2).
           03  QFD-FIELDED-44     PIC 9(2).
           03  QFD-FIELDKEY-44    PIC 9(4).
           03  QFD-FIELDNO-45     PIC 9(4).
           03  QFD-FIELDROW-45    PIC 9(2).
           03  QFD-FIELDCOL-45    PIC 9(2).
           03  QFD-FIELDED-45     PIC 9(2).
           03  QFD-FIELDKEY-45    PIC 9(4).
           03  QFD-FIELDNO-46     PIC 9(4).
           03  QFD-FIELDROW-46    PIC 9(2).
           03  QFD-FIELDCOL-46    PIC 9(2).
           03  QFD-FIELDED-46     PIC 9(2).
           03  QFD-FIELDKEY-46    PIC 9(4).
           03  QFD-FIELDNO-47     PIC 9(4).
           03  QFD-FIELDROW-47    PIC 9(2).
           03  QFD-FIELDCOL-47    PIC 9(2).
           03  QFD-FIELDED-47     PIC 9(2).
           03  QFD-FIELDKEY-47    PIC 9(4).
           03  QFD-FIELDNO-48     PIC 9(4).
           03  QFD-FIELDROW-48    PIC 9(2).
           03  QFD-FIELDCOL-48    PIC 9(2).
           03  QFD-FIELDED-48     PIC 9(2).
           03  QFD-FIELDKEY-48    PIC 9(4).
           03  QFD-FIELDNO-49     PIC 9(4).
           03  QFD-FIELDROW-49    PIC 9(2).
           03  QFD-FIELDCOL-49    PIC 9(2).
           03  QFD-FIELDED-49     PIC 9(2).
           03  QFD-FIELDKEY-49    PIC 9(4).
           03  QFD-FIELDNO-50     PIC 9(4).
           03  QFD-FIELDROW-50    PIC 9(2).
           03  QFD-FIELDCOL-50    PIC 9(2).
           03  QFD-FIELDED-50     PIC 9(2).
           03  QFD-FIELDKEY-50    PIC 9(4).
           03  QFD-1STFILE        PIC 9(2).
           03  QFD-INCR-DELCNT          PIC X.
           03  QFD-FMTYPE               PIC X.
           03  QFD-PRCONT               PIC X.
           03  QFD-NEXTFM               PIC 9(4).
           03  QFD-VALID-BRANCH         PIC X(4).
           03  QFD-MIN-FORM-SENT-DAYS   PIC 99.
           03  QFD-SKIP-BAD-ADR         PIC X.
           03  QFD-SECURITY-PROFILE     PIC X.
           03  QFD-FORM-RIGHT-TO-CURE   PIC X.
           03  QFD-TEST-WHICH-STATE     PIC X.
           03  QFD-STATE-1              PIC XX.
           03  QFD-STATE-2              PIC XX.
           03  QFD-STATE-3              PIC XX.
           03  QFD-STATE-4              PIC XX.
           03  QFD-STATE-5              PIC XX.
           03  QFD-STATE-6              PIC XX.
           03  QFD-STATE-7              PIC XX.
           03  QFD-STATE-8              PIC XX.
           03  QFD-STATE-9              PIC XX.
           03  QFD-STATE-10             PIC XX.
           03  QFD-STATE-11             PIC XX.
           03  QFD-STATE-12             PIC XX.
           03  QFD-STATE-13             PIC XX.
           03  QFD-STATE-14             PIC XX.
           03  QFD-STATE-15             PIC XX.
           03  QFD-STATE-16             PIC XX.
           03  QFD-STATE-17             PIC XX.
           03  QFD-STATE-18             PIC XX.
           03  QFD-STATE-19             PIC XX.
           03  QFD-STATE-20             PIC XX.
           03  QFD-STATE-21             PIC XX.
           03  QFD-STATE-22             PIC XX.
           03  QFD-STATE-23             PIC XX.
           03  QFD-STATE-24             PIC XX.
           03  QFD-STATE-25             PIC XX.
           03  QFD-STATE-26             PIC XX.
           03  QFD-STATE-27             PIC XX.
           03  QFD-STATE-28             PIC XX.
           03  QFD-STATE-29             PIC XX.
           03  QFD-STATE-30             PIC XX.
           03  QFD-STATE-31             PIC XX.
           03  QFD-STATE-32             PIC XX.
           03  QFD-STATE-33             PIC XX.
           03  QFD-STATE-34             PIC XX.
           03  QFD-STATE-35             PIC XX.
           03  QFD-STATE-36             PIC XX.
           03  QFD-STATE-37             PIC XX.
           03  QFD-STATE-38             PIC XX.
           03  QFD-STATE-39             PIC XX.
           03  QFD-STATE-40             PIC XX.
           03  QFD-STATE-41             PIC XX.
           03  QFD-STATE-42             PIC XX.
           03  QFD-STATE-43             PIC XX.
           03  QFD-STATE-44             PIC XX.
           03  QFD-STATE-45             PIC XX.
           03  QFD-STATE-46             PIC XX.
           03  QFD-STATE-47             PIC XX.
           03  QFD-STATE-48             PIC XX.
           03  QFD-STATE-49             PIC XX.
           03  QFD-STATE-50             PIC XX.

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-FMNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING FMNO       (F3-ALL) ".

       01  LEGEND-END-FMNO.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING FMNO       ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-FMNO                PIC X(04).
           03  END-FMNO                PIC X(04).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "FMNO LAST TOUCH DESC          ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-FD-FMNO                    PIC X(04).
           03  FILLER                         PIC X(02).
           03  D01-FD-LTOUCH-DATE             PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-FD-DESC                    PIC X(25).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOFDRN_DEF.CPY".
       COPY "LIBIO/IOFDRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOFDRN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                        TO FD-PATH-OVERRIDE.

           MOVE BEG-FMNO                   TO QFD1-WBEG-FMNO.
           MOVE END-FMNO                   TO QFD1-WEND-FMNO.

           PERFORM OPEN-FD1-FILE.
           PERFORM START-FD1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-FD1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( FD-FMNO < BEG-FMNO )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( FD-FMNO > END-FMNO )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FD-FMNO                    TO D01-FD-FMNO.
           MOVE FD-LTOUCH-DATE             TO D01-FD-LTOUCH-DATE.
           MOVE FD-DESC                    TO D01-FD-DESC.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOFDRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOFDRN-QUEUE-POS  TO Q-POS.
           MOVE IOFDRN-COPIES-POS TO C-POS.
           MOVE IOFDRN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-FMNO.

           MOVE LEGEND-BEG-FMNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-FMNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-FMNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-FMNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-FMNO. 

      *-----------------------------------------------------------------
       ENTER-END-FMNO. 

           MOVE LEGEND-END-FMNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-FMNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-FMNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-FMNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-FMNO.

           IF ( BEG-FMNO > END-FMNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-FMNO.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-FMNO.

           MOVE "S  A040BEG-FMNO." TO VDU.
           MOVE SPACES               TO BEG-FMNO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-FMNO." TO VDU.
           MOVE ALL "Z"              TO END-FMNO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-FMNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )      
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           MOVE "S  A000BEG-FMNO."    TO VDU.
           MOVE BEG-FMNO              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-FMNO."    TO VDU.
           MOVE END-FMNO              TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE FD1-KEY     TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING FMNO                : "    TO R01-PROMPT.
           MOVE BEG-FMNO                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    FMNO                : "    TO R01-PROMPT.
           MOVE END-FMNO                              TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
      *    MOVE BEG-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
      *    MOVE END-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - FDFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOFDRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOFDRN-SYSDATE.
           DISPLAY IOFDRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOFDRN-FORM-FIELDS TO WR-BUF.
           MOVE IOFDRN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOFDRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOFDRN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOFDRN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPYMEMBER: LIBLP/LPFD1RN

       LOAD-FD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( FD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( FD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO FD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FD-ALL-MACHINE
              MOVE FD-ALL-PATH             TO FD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( FD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO FD-ALL-BASE
              MOVE FILPATH-MACHINE         TO FD-ALL-MACHINE
              MOVE FD-ALL-PATH             TO FD-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-FDFILE NOT = "S" )
              MOVE FD-PATH-BEG             TO FD-SELECT-PATH-BEG
              MOVE FD-PATH-FILE            TO FD-SELECT-PATH-FILE
           ELSE
              MOVE FD-PATH-SQL             TO FD-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-FD1-FILE.
           PERFORM LOAD-FD1-FILE.
           PERFORM OPEN-IT.
           MOVE FD-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-FD1-FILE.
           PERFORM START-IT.
           MOVE FD-SELECT-PATH TO E-FILE.
           MOVE FD1-KEY TO E-KEYX.

           IF ( FD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-FD1-FILE.

           IF ( QFD1-WEND-FMNO = ZEROES    ) OR
              ( QFD1-WEND-FMNO NOT NUMERIC )
              MOVE FD-FMNO        TO QFD1-WBEG-FMNO
              MOVE ALL "9"        TO QFD1-WEND-FMNO.

           EXEC SQL
            DECLARE FD1_CURSOR CURSOR FOR
            SELECT FDFILE.FD_FMNO,
                   ISNULL(CAST(FDFILE.FD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(FDFILE.FD_DESC,''),
                   ISNULL(FDFILE.FD_OPTION,''),
                   ISNULL(FDFILE.FD_FORM_CHG_CD,''),
                   ISNULL(FDFILE.FD_PROG_PREFIX,''),
                   ISNULL(FDFILE.FD_TYPE,''),
                   ISNULL(FDFILE.FD_LNMEMO,''),
                   FDFILE.FD_BODY,
                   FDFILE.FD_TOP,
                   FDFILE.FD_BOTTOM,
                   ISNULL(FDFILE.FD_PROG,''),
                   ISNULL(FDFILE.FD_PROMPTS_1,''),
                   ISNULL(FDFILE.FD_PROMPTS_2,''),
                   ISNULL(FDFILE.FD_PROMPTS_3,''),
                   ISNULL(FDFILE.FD_PROMPTS_4,''),
                   ISNULL(FDFILE.FD_PROMPTS_5,''),
                   ISNULL(FDFILE.FD_PROMPTS_6,''),
                   ISNULL(FDFILE.FD_PROMPTS_7,''),
                   ISNULL(FDFILE.FD_PROMPTS_8,''),
                   ISNULL(FDFILE.FD_PROMPTS_9,''),
                   ISNULL(FDFILE.FD_PROMPTS_10,''),
                   ISNULL(FDFILE.FD_PROMPTS_11,''),
                   ISNULL(FDFILE.FD_PROMPTS_12,''),
                   ISNULL(FDFILE.FD_PROMPTS_13,''),
                   ISNULL(FDFILE.FD_PROMPTS_14,''),
                   ISNULL(FDFILE.FD_PROMPTS_15,''),
                   ISNULL(FDFILE.FD_PROMPTS_16,''),
                   ISNULL(FDFILE.FD_PROMPTS_17,''),
                   ISNULL(FDFILE.FD_PROMPTS_18,''),
                   ISNULL(FDFILE.FD_PROMPTS_19,''),
                   ISNULL(FDFILE.FD_PROMPTS_20,''),
                   ISNULL(FDFILE.FD_PROMPTS_21,''),
                   ISNULL(FDFILE.FD_PROMPTS_22,''),
                   ISNULL(FDFILE.FD_PROMPTS_23,''),
                   ISNULL(FDFILE.FD_PROMPTS_24,''),
                   ISNULL(FDFILE.FD_PROMPTS_25,''),
                   ISNULL(FDFILE.FD_PROMPTS_26,''),
                   ISNULL(FDFILE.FD_PROMPTS_27,''),
                   ISNULL(FDFILE.FD_PROMPTS_28,''),
                   ISNULL(FDFILE.FD_PROMPTS_29,''),
                   ISNULL(FDFILE.FD_PROMPTS_30,''),
                   FDFILE.FD_FIELDNO_1,
                   FDFILE.FD_FIELDROW_1,
                   FDFILE.FD_FIELDCOL_1,
                   FDFILE.FD_FIELDED_1,
                   FDFILE.FD_FIELDKEY_1,
                   FDFILE.FD_FIELDNO_2,
                   FDFILE.FD_FIELDROW_2,
                   FDFILE.FD_FIELDCOL_2,
                   FDFILE.FD_FIELDED_2,
                   FDFILE.FD_FIELDKEY_2,
                   FDFILE.FD_FIELDNO_3,
                   FDFILE.FD_FIELDROW_3,
                   FDFILE.FD_FIELDCOL_3,
                   FDFILE.FD_FIELDED_3,
                   FDFILE.FD_FIELDKEY_3,
                   FDFILE.FD_FIELDNO_4,
                   FDFILE.FD_FIELDROW_4,
                   FDFILE.FD_FIELDCOL_4,
                   FDFILE.FD_FIELDED_4,
                   FDFILE.FD_FIELDKEY_4,
                   FDFILE.FD_FIELDNO_5,
                   FDFILE.FD_FIELDROW_5,
                   FDFILE.FD_FIELDCOL_5,
                   FDFILE.FD_FIELDED_5,
                   FDFILE.FD_FIELDKEY_5,
                   FDFILE.FD_FIELDNO_6,
                   FDFILE.FD_FIELDROW_6,
                   FDFILE.FD_FIELDCOL_6,
                   FDFILE.FD_FIELDED_6,
                   FDFILE.FD_FIELDKEY_6,
                   FDFILE.FD_FIELDNO_7,
                   FDFILE.FD_FIELDROW_7,
                   FDFILE.FD_FIELDCOL_7,
                   FDFILE.FD_FIELDED_7,
                   FDFILE.FD_FIELDKEY_7,
                   FDFILE.FD_FIELDNO_8,
                   FDFILE.FD_FIELDROW_8,
                   FDFILE.FD_FIELDCOL_8,
                   FDFILE.FD_FIELDED_8,
                   FDFILE.FD_FIELDKEY_8,
                   FDFILE.FD_FIELDNO_9,
                   FDFILE.FD_FIELDROW_9,
                   FDFILE.FD_FIELDCOL_9,
                   FDFILE.FD_FIELDED_9,
                   FDFILE.FD_FIELDKEY_9,
                   FDFILE.FD_FIELDNO_10,
                   FDFILE.FD_FIELDROW_10,
                   FDFILE.FD_FIELDCOL_10,
                   FDFILE.FD_FIELDED_10,
                   FDFILE.FD_FIELDKEY_10,
                   FDFILE.FD_FIELDNO_11,
                   FDFILE.FD_FIELDROW_11,
                   FDFILE.FD_FIELDCOL_11,
                   FDFILE.FD_FIELDED_11,
                   FDFILE.FD_FIELDKEY_11,
                   FDFILE.FD_FIELDNO_12,
                   FDFILE.FD_FIELDROW_12,
                   FDFILE.FD_FIELDCOL_12,
                   FDFILE.FD_FIELDED_12,
                   FDFILE.FD_FIELDKEY_12,
                   FDFILE.FD_FIELDNO_13,
                   FDFILE.FD_FIELDROW_13,
                   FDFILE.FD_FIELDCOL_13,
                   FDFILE.FD_FIELDED_13,
                   FDFILE.FD_FIELDKEY_13,
                   FDFILE.FD_FIELDNO_14,
                   FDFILE.FD_FIELDROW_14,
                   FDFILE.FD_FIELDCOL_14,
                   FDFILE.FD_FIELDED_14,
                   FDFILE.FD_FIELDKEY_14,
                   FDFILE.FD_FIELDNO_15,
                   FDFILE.FD_FIELDROW_15,
                   FDFILE.FD_FIELDCOL_15,
                   FDFILE.FD_FIELDED_15,
                   FDFILE.FD_FIELDKEY_15,
                   FDFILE.FD_FIELDNO_16,
                   FDFILE.FD_FIELDROW_16,
                   FDFILE.FD_FIELDCOL_16,
                   FDFILE.FD_FIELDED_16,
                   FDFILE.FD_FIELDKEY_16,
                   FDFILE.FD_FIELDNO_17,
                   FDFILE.FD_FIELDROW_17,
                   FDFILE.FD_FIELDCOL_17,
                   FDFILE.FD_FIELDED_17,
                   FDFILE.FD_FIELDKEY_17,
                   FDFILE.FD_FIELDNO_18,
                   FDFILE.FD_FIELDROW_18,
                   FDFILE.FD_FIELDCOL_18,
                   FDFILE.FD_FIELDED_18,
                   FDFILE.FD_FIELDKEY_18,
                   FDFILE.FD_FIELDNO_19,
                   FDFILE.FD_FIELDROW_19,
                   FDFILE.FD_FIELDCOL_19,
                   FDFILE.FD_FIELDED_19,
                   FDFILE.FD_FIELDKEY_19,
                   FDFILE.FD_FIELDNO_20,
                   FDFILE.FD_FIELDROW_20,
                   FDFILE.FD_FIELDCOL_20,
                   FDFILE.FD_FIELDED_20,
                   FDFILE.FD_FIELDKEY_20,
                   FDFILE.FD_FIELDNO_21,
                   FDFILE.FD_FIELDROW_21,
                   FDFILE.FD_FIELDCOL_21,
                   FDFILE.FD_FIELDED_21,
                   FDFILE.FD_FIELDKEY_21,
                   FDFILE.FD_FIELDNO_22,
                   FDFILE.FD_FIELDROW_22,
                   FDFILE.FD_FIELDCOL_22,
                   FDFILE.FD_FIELDED_22,
                   FDFILE.FD_FIELDKEY_22,
                   FDFILE.FD_FIELDNO_23,
                   FDFILE.FD_FIELDROW_23,
                   FDFILE.FD_FIELDCOL_23,
                   FDFILE.FD_FIELDED_23,
                   FDFILE.FD_FIELDKEY_23,
                   FDFILE.FD_FIELDNO_24,
                   FDFILE.FD_FIELDROW_24,
                   FDFILE.FD_FIELDCOL_24,
                   FDFILE.FD_FIELDED_24,
                   FDFILE.FD_FIELDKEY_24,
                   FDFILE.FD_FIELDNO_25,
                   FDFILE.FD_FIELDROW_25,
                   FDFILE.FD_FIELDCOL_25,
                   FDFILE.FD_FIELDED_25,
                   FDFILE.FD_FIELDKEY_25,
                   FDFILE.FD_FIELDNO_26,
                   FDFILE.FD_FIELDROW_26,
                   FDFILE.FD_FIELDCOL_26,
                   FDFILE.FD_FIELDED_26,
                   FDFILE.FD_FIELDKEY_26,
                   FDFILE.FD_FIELDNO_27,
                   FDFILE.FD_FIELDROW_27,
                   FDFILE.FD_FIELDCOL_27,
                   FDFILE.FD_FIELDED_27,
                   FDFILE.FD_FIELDKEY_27,
                   FDFILE.FD_FIELDNO_28,
                   FDFILE.FD_FIELDROW_28,
                   FDFILE.FD_FIELDCOL_28,
                   FDFILE.FD_FIELDED_28,
                   FDFILE.FD_FIELDKEY_28,
                   FDFILE.FD_FIELDNO_29,
                   FDFILE.FD_FIELDROW_29,
                   FDFILE.FD_FIELDCOL_29,
                   FDFILE.FD_FIELDED_29,
                   FDFILE.FD_FIELDKEY_29,
                   FDFILE.FD_FIELDNO_30,
                   FDFILE.FD_FIELDROW_30,
                   FDFILE.FD_FIELDCOL_30,
                   FDFILE.FD_FIELDED_30,
                   FDFILE.FD_FIELDKEY_30,
                   FDFILE.FD_FIELDNO_31,
                   FDFILE.FD_FIELDROW_31,
                   FDFILE.FD_FIELDCOL_31,
                   FDFILE.FD_FIELDED_31,
                   FDFILE.FD_FIELDKEY_31,
                   FDFILE.FD_FIELDNO_32,
                   FDFILE.FD_FIELDROW_32,
                   FDFILE.FD_FIELDCOL_32,
                   FDFILE.FD_FIELDED_32,
                   FDFILE.FD_FIELDKEY_32,
                   FDFILE.FD_FIELDNO_33,
                   FDFILE.FD_FIELDROW_33,
                   FDFILE.FD_FIELDCOL_33,
                   FDFILE.FD_FIELDED_33,
                   FDFILE.FD_FIELDKEY_33,
                   FDFILE.FD_FIELDNO_34,
                   FDFILE.FD_FIELDROW_34,
                   FDFILE.FD_FIELDCOL_34,
                   FDFILE.FD_FIELDED_34,
                   FDFILE.FD_FIELDKEY_34,
                   FDFILE.FD_FIELDNO_35,
                   FDFILE.FD_FIELDROW_35,
                   FDFILE.FD_FIELDCOL_35,
                   FDFILE.FD_FIELDED_35,
                   FDFILE.FD_FIELDKEY_35,
                   FDFILE.FD_FIELDNO_36,
                   FDFILE.FD_FIELDROW_36,
                   FDFILE.FD_FIELDCOL_36,
                   FDFILE.FD_FIELDED_36,
                   FDFILE.FD_FIELDKEY_36,
                   FDFILE.FD_FIELDNO_37,
                   FDFILE.FD_FIELDROW_37,
                   FDFILE.FD_FIELDCOL_37,
                   FDFILE.FD_FIELDED_37,
                   FDFILE.FD_FIELDKEY_37,
                   FDFILE.FD_FIELDNO_38,
                   FDFILE.FD_FIELDROW_38,
                   FDFILE.FD_FIELDCOL_38,
                   FDFILE.FD_FIELDED_38,
                   FDFILE.FD_FIELDKEY_38,
                   FDFILE.FD_FIELDNO_39,
                   FDFILE.FD_FIELDROW_39,
                   FDFILE.FD_FIELDCOL_39,
                   FDFILE.FD_FIELDED_39,
                   FDFILE.FD_FIELDKEY_39,
                   FDFILE.FD_FIELDNO_40,
                   FDFILE.FD_FIELDROW_40,
                   FDFILE.FD_FIELDCOL_40,
                   FDFILE.FD_FIELDED_40,
                   FDFILE.FD_FIELDKEY_40,
                   FDFILE.FD_FIELDNO_41,
                   FDFILE.FD_FIELDROW_41,
                   FDFILE.FD_FIELDCOL_41,
                   FDFILE.FD_FIELDED_41,
                   FDFILE.FD_FIELDKEY_41,
                   FDFILE.FD_FIELDNO_42,
                   FDFILE.FD_FIELDROW_42,
                   FDFILE.FD_FIELDCOL_42,
                   FDFILE.FD_FIELDED_42,
                   FDFILE.FD_FIELDKEY_42,
                   FDFILE.FD_FIELDNO_43,
                   FDFILE.FD_FIELDROW_43,
                   FDFILE.FD_FIELDCOL_43,
                   FDFILE.FD_FIELDED_43,
                   FDFILE.FD_FIELDKEY_43,
                   FDFILE.FD_FIELDNO_44,
                   FDFILE.FD_FIELDROW_44,
                   FDFILE.FD_FIELDCOL_44,
                   FDFILE.FD_FIELDED_44,
                   FDFILE.FD_FIELDKEY_44,
                   FDFILE.FD_FIELDNO_45,
                   FDFILE.FD_FIELDROW_45,
                   FDFILE.FD_FIELDCOL_45,
                   FDFILE.FD_FIELDED_45,
                   FDFILE.FD_FIELDKEY_45,
                   FDFILE.FD_FIELDNO_46,
                   FDFILE.FD_FIELDROW_46,
                   FDFILE.FD_FIELDCOL_46,
                   FDFILE.FD_FIELDED_46,
                   FDFILE.FD_FIELDKEY_46,
                   FDFILE.FD_FIELDNO_47,
                   FDFILE.FD_FIELDROW_47,
                   FDFILE.FD_FIELDCOL_47,
                   FDFILE.FD_FIELDED_47,
                   FDFILE.FD_FIELDKEY_47,
                   FDFILE.FD_FIELDNO_48,
                   FDFILE.FD_FIELDROW_48,
                   FDFILE.FD_FIELDCOL_48,
                   FDFILE.FD_FIELDED_48,
                   FDFILE.FD_FIELDKEY_48,
                   FDFILE.FD_FIELDNO_49,
                   FDFILE.FD_FIELDROW_49,
                   FDFILE.FD_FIELDCOL_49,
                   FDFILE.FD_FIELDED_49,
                   FDFILE.FD_FIELDKEY_49,
                   FDFILE.FD_FIELDNO_50,
                   FDFILE.FD_FIELDROW_50,
                   FDFILE.FD_FIELDCOL_50,
                   FDFILE.FD_FIELDED_50,
                   FDFILE.FD_FIELDKEY_50,
                   FDFILE.FD_1STFILE,
                   ISNULL(FDFILE.FD_INCR_DELCNT,''),
                   ISNULL(FDFILE.FD_FMTYPE,''),
                   ISNULL(FDFILE.FD_PRCONT,''),
                   FDFILE.FD_NEXTFM,
                   ISNULL(FDFILE.FD_VALID_BRANCH,''),
                   FDFILE.FD_MIN_FORM_SENT_DAYS,
                   ISNULL(FDFILE.FD_SKIP_BAD_ADR,''),
                   ISNULL(FDFILE.FD_SECURITY_PROFILE,''),
                   ISNULL(FDFILE.FD_FORM_RIGHT_TO_CURE,''),
                   ISNULL(FDFILE.FD_TEST_WHICH_STATE,''),
                   ISNULL(FDFILE.FD_STATE_1,''),
                   ISNULL(FDFILE.FD_STATE_2,''),
                   ISNULL(FDFILE.FD_STATE_3,''),
                   ISNULL(FDFILE.FD_STATE_4,''),
                   ISNULL(FDFILE.FD_STATE_5,''),
                   ISNULL(FDFILE.FD_STATE_6,''),
                   ISNULL(FDFILE.FD_STATE_7,''),
                   ISNULL(FDFILE.FD_STATE_8,''),
                   ISNULL(FDFILE.FD_STATE_9,''),
                   ISNULL(FDFILE.FD_STATE_10,''),
                   ISNULL(FDFILE.FD_STATE_11,''),
                   ISNULL(FDFILE.FD_STATE_12,''),
                   ISNULL(FDFILE.FD_STATE_13,''),
                   ISNULL(FDFILE.FD_STATE_14,''),
                   ISNULL(FDFILE.FD_STATE_15,''),
                   ISNULL(FDFILE.FD_STATE_16,''),
                   ISNULL(FDFILE.FD_STATE_17,''),
                   ISNULL(FDFILE.FD_STATE_18,''),
                   ISNULL(FDFILE.FD_STATE_19,''),
                   ISNULL(FDFILE.FD_STATE_20,''),
                   ISNULL(FDFILE.FD_STATE_21,''),
                   ISNULL(FDFILE.FD_STATE_22,''),
                   ISNULL(FDFILE.FD_STATE_23,''),
                   ISNULL(FDFILE.FD_STATE_24,''),
                   ISNULL(FDFILE.FD_STATE_25,''),
                   ISNULL(FDFILE.FD_STATE_26,''),
                   ISNULL(FDFILE.FD_STATE_27,''),
                   ISNULL(FDFILE.FD_STATE_28,''),
                   ISNULL(FDFILE.FD_STATE_29,''),
                   ISNULL(FDFILE.FD_STATE_30,''),
                   ISNULL(FDFILE.FD_STATE_31,''),
                   ISNULL(FDFILE.FD_STATE_32,''),
                   ISNULL(FDFILE.FD_STATE_33,''),
                   ISNULL(FDFILE.FD_STATE_34,''),
                   ISNULL(FDFILE.FD_STATE_35,''),
                   ISNULL(FDFILE.FD_STATE_36,''),
                   ISNULL(FDFILE.FD_STATE_37,''),
                   ISNULL(FDFILE.FD_STATE_38,''),
                   ISNULL(FDFILE.FD_STATE_39,''),
                   ISNULL(FDFILE.FD_STATE_40,''),
                   ISNULL(FDFILE.FD_STATE_41,''),
                   ISNULL(FDFILE.FD_STATE_42,''),
                   ISNULL(FDFILE.FD_STATE_43,''),
                   ISNULL(FDFILE.FD_STATE_44,''),
                   ISNULL(FDFILE.FD_STATE_45,''),
                   ISNULL(FDFILE.FD_STATE_46,''),
                   ISNULL(FDFILE.FD_STATE_47,''),
                   ISNULL(FDFILE.FD_STATE_48,''),
                   ISNULL(FDFILE.FD_STATE_49,''),
                   ISNULL(FDFILE.FD_STATE_50,'')
            FROM DBO.FDFILE
            WHERE FDFILE.FD_FMNO >= :QFD1-WBEG-FMNO
              AND FDFILE.FD_FMNO <= :QFD1-WEND-FMNO
            ORDER BY FDFILE.FD_FMNO
           END-EXEC.

           EXEC SQL
               OPEN FD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QFD1-WBEG-FMNO
                          QFD1-WEND-FMNO.

           MOVE STAT---GOOD TO FD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-FD1-FILE.
           PERFORM READ-IT.
           MOVE FD-SELECT-PATH TO E-FILE.
           MOVE FD1-KEY TO E-KEYX.

           MOVE FD-FMNO    TO QFD-FMNO.  

           EXEC SQL
            SELECT FDFILE.FD_FMNO,
                   ISNULL(CAST(FDFILE.FD_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(FDFILE.FD_DESC,''),
                   ISNULL(FDFILE.FD_OPTION,''),
                   ISNULL(FDFILE.FD_FORM_CHG_CD,''),
                   ISNULL(FDFILE.FD_PROG_PREFIX,''),
                   ISNULL(FDFILE.FD_TYPE,''),
                   ISNULL(FDFILE.FD_LNMEMO,''),
                   FDFILE.FD_BODY,
                   FDFILE.FD_TOP,
                   FDFILE.FD_BOTTOM,
                   ISNULL(FDFILE.FD_PROG,''),
                   ISNULL(FDFILE.FD_PROMPTS_1,''),
                   ISNULL(FDFILE.FD_PROMPTS_2,''),
                   ISNULL(FDFILE.FD_PROMPTS_3,''),
                   ISNULL(FDFILE.FD_PROMPTS_4,''),
                   ISNULL(FDFILE.FD_PROMPTS_5,''),
                   ISNULL(FDFILE.FD_PROMPTS_6,''),
                   ISNULL(FDFILE.FD_PROMPTS_7,''),
                   ISNULL(FDFILE.FD_PROMPTS_8,''),
                   ISNULL(FDFILE.FD_PROMPTS_9,''),
                   ISNULL(FDFILE.FD_PROMPTS_10,''),
                   ISNULL(FDFILE.FD_PROMPTS_11,''),
                   ISNULL(FDFILE.FD_PROMPTS_12,''),
                   ISNULL(FDFILE.FD_PROMPTS_13,''),
                   ISNULL(FDFILE.FD_PROMPTS_14,''),
                   ISNULL(FDFILE.FD_PROMPTS_15,''),
                   ISNULL(FDFILE.FD_PROMPTS_16,''),
                   ISNULL(FDFILE.FD_PROMPTS_17,''),
                   ISNULL(FDFILE.FD_PROMPTS_18,''),
                   ISNULL(FDFILE.FD_PROMPTS_19,''),
                   ISNULL(FDFILE.FD_PROMPTS_20,''),
                   ISNULL(FDFILE.FD_PROMPTS_21,''),
                   ISNULL(FDFILE.FD_PROMPTS_22,''),
                   ISNULL(FDFILE.FD_PROMPTS_23,''),
                   ISNULL(FDFILE.FD_PROMPTS_24,''),
                   ISNULL(FDFILE.FD_PROMPTS_25,''),
                   ISNULL(FDFILE.FD_PROMPTS_26,''),
                   ISNULL(FDFILE.FD_PROMPTS_27,''),
                   ISNULL(FDFILE.FD_PROMPTS_28,''),
                   ISNULL(FDFILE.FD_PROMPTS_29,''),
                   ISNULL(FDFILE.FD_PROMPTS_30,''),
                   FDFILE.FD_FIELDNO_1,
                   FDFILE.FD_FIELDROW_1,
                   FDFILE.FD_FIELDCOL_1,
                   FDFILE.FD_FIELDED_1,
                   FDFILE.FD_FIELDKEY_1,
                   FDFILE.FD_FIELDNO_2,
                   FDFILE.FD_FIELDROW_2,
                   FDFILE.FD_FIELDCOL_2,
                   FDFILE.FD_FIELDED_2,
                   FDFILE.FD_FIELDKEY_2,
                   FDFILE.FD_FIELDNO_3,
                   FDFILE.FD_FIELDROW_3,
                   FDFILE.FD_FIELDCOL_3,
                   FDFILE.FD_FIELDED_3,
                   FDFILE.FD_FIELDKEY_3,
                   FDFILE.FD_FIELDNO_4,
                   FDFILE.FD_FIELDROW_4,
                   FDFILE.FD_FIELDCOL_4,
                   FDFILE.FD_FIELDED_4,
                   FDFILE.FD_FIELDKEY_4,
                   FDFILE.FD_FIELDNO_5,
                   FDFILE.FD_FIELDROW_5,
                   FDFILE.FD_FIELDCOL_5,
                   FDFILE.FD_FIELDED_5,
                   FDFILE.FD_FIELDKEY_5,
                   FDFILE.FD_FIELDNO_6,
                   FDFILE.FD_FIELDROW_6,
                   FDFILE.FD_FIELDCOL_6,
                   FDFILE.FD_FIELDED_6,
                   FDFILE.FD_FIELDKEY_6,
                   FDFILE.FD_FIELDNO_7,
                   FDFILE.FD_FIELDROW_7,
                   FDFILE.FD_FIELDCOL_7,
                   FDFILE.FD_FIELDED_7,
                   FDFILE.FD_FIELDKEY_7,
                   FDFILE.FD_FIELDNO_8,
                   FDFILE.FD_FIELDROW_8,
                   FDFILE.FD_FIELDCOL_8,
                   FDFILE.FD_FIELDED_8,
                   FDFILE.FD_FIELDKEY_8,
                   FDFILE.FD_FIELDNO_9,
                   FDFILE.FD_FIELDROW_9,
                   FDFILE.FD_FIELDCOL_9,
                   FDFILE.FD_FIELDED_9,
                   FDFILE.FD_FIELDKEY_9,
                   FDFILE.FD_FIELDNO_10,
                   FDFILE.FD_FIELDROW_10,
                   FDFILE.FD_FIELDCOL_10,
                   FDFILE.FD_FIELDED_10,
                   FDFILE.FD_FIELDKEY_10,
                   FDFILE.FD_FIELDNO_11,
                   FDFILE.FD_FIELDROW_11,
                   FDFILE.FD_FIELDCOL_11,
                   FDFILE.FD_FIELDED_11,
                   FDFILE.FD_FIELDKEY_11,
                   FDFILE.FD_FIELDNO_12,
                   FDFILE.FD_FIELDROW_12,
                   FDFILE.FD_FIELDCOL_12,
                   FDFILE.FD_FIELDED_12,
                   FDFILE.FD_FIELDKEY_12,
                   FDFILE.FD_FIELDNO_13,
                   FDFILE.FD_FIELDROW_13,
                   FDFILE.FD_FIELDCOL_13,
                   FDFILE.FD_FIELDED_13,
                   FDFILE.FD_FIELDKEY_13,
                   FDFILE.FD_FIELDNO_14,
                   FDFILE.FD_FIELDROW_14,
                   FDFILE.FD_FIELDCOL_14,
                   FDFILE.FD_FIELDED_14,
                   FDFILE.FD_FIELDKEY_14,
                   FDFILE.FD_FIELDNO_15,
                   FDFILE.FD_FIELDROW_15,
                   FDFILE.FD_FIELDCOL_15,
                   FDFILE.FD_FIELDED_15,
                   FDFILE.FD_FIELDKEY_15,
                   FDFILE.FD_FIELDNO_16,
                   FDFILE.FD_FIELDROW_16,
                   FDFILE.FD_FIELDCOL_16,
                   FDFILE.FD_FIELDED_16,
                   FDFILE.FD_FIELDKEY_16,
                   FDFILE.FD_FIELDNO_17,
                   FDFILE.FD_FIELDROW_17,
                   FDFILE.FD_FIELDCOL_17,
                   FDFILE.FD_FIELDED_17,
                   FDFILE.FD_FIELDKEY_17,
                   FDFILE.FD_FIELDNO_18,
                   FDFILE.FD_FIELDROW_18,
                   FDFILE.FD_FIELDCOL_18,
                   FDFILE.FD_FIELDED_18,
                   FDFILE.FD_FIELDKEY_18,
                   FDFILE.FD_FIELDNO_19,
                   FDFILE.FD_FIELDROW_19,
                   FDFILE.FD_FIELDCOL_19,
                   FDFILE.FD_FIELDED_19,
                   FDFILE.FD_FIELDKEY_19,
                   FDFILE.FD_FIELDNO_20,
                   FDFILE.FD_FIELDROW_20,
                   FDFILE.FD_FIELDCOL_20,
                   FDFILE.FD_FIELDED_20,
                   FDFILE.FD_FIELDKEY_20,
                   FDFILE.FD_FIELDNO_21,
                   FDFILE.FD_FIELDROW_21,
                   FDFILE.FD_FIELDCOL_21,
                   FDFILE.FD_FIELDED_21,
                   FDFILE.FD_FIELDKEY_21,
                   FDFILE.FD_FIELDNO_22,
                   FDFILE.FD_FIELDROW_22,
                   FDFILE.FD_FIELDCOL_22,
                   FDFILE.FD_FIELDED_22,
                   FDFILE.FD_FIELDKEY_22,
                   FDFILE.FD_FIELDNO_23,
                   FDFILE.FD_FIELDROW_23,
                   FDFILE.FD_FIELDCOL_23,
                   FDFILE.FD_FIELDED_23,
                   FDFILE.FD_FIELDKEY_23,
                   FDFILE.FD_FIELDNO_24,
                   FDFILE.FD_FIELDROW_24,
                   FDFILE.FD_FIELDCOL_24,
                   FDFILE.FD_FIELDED_24,
                   FDFILE.FD_FIELDKEY_24,
                   FDFILE.FD_FIELDNO_25,
                   FDFILE.FD_FIELDROW_25,
                   FDFILE.FD_FIELDCOL_25,
                   FDFILE.FD_FIELDED_25,
                   FDFILE.FD_FIELDKEY_25,
                   FDFILE.FD_FIELDNO_26,
                   FDFILE.FD_FIELDROW_26,
                   FDFILE.FD_FIELDCOL_26,
                   FDFILE.FD_FIELDED_26,
                   FDFILE.FD_FIELDKEY_26,
                   FDFILE.FD_FIELDNO_27,
                   FDFILE.FD_FIELDROW_27,
                   FDFILE.FD_FIELDCOL_27,
                   FDFILE.FD_FIELDED_27,
                   FDFILE.FD_FIELDKEY_27,
                   FDFILE.FD_FIELDNO_28,
                   FDFILE.FD_FIELDROW_28,
                   FDFILE.FD_FIELDCOL_28,
                   FDFILE.FD_FIELDED_28,
                   FDFILE.FD_FIELDKEY_28,
                   FDFILE.FD_FIELDNO_29,
                   FDFILE.FD_FIELDROW_29,
                   FDFILE.FD_FIELDCOL_29,
                   FDFILE.FD_FIELDED_29,
                   FDFILE.FD_FIELDKEY_29,
                   FDFILE.FD_FIELDNO_30,
                   FDFILE.FD_FIELDROW_30,
                   FDFILE.FD_FIELDCOL_30,
                   FDFILE.FD_FIELDED_30,
                   FDFILE.FD_FIELDKEY_30,
                   FDFILE.FD_FIELDNO_31,
                   FDFILE.FD_FIELDROW_31,
                   FDFILE.FD_FIELDCOL_31,
                   FDFILE.FD_FIELDED_31,
                   FDFILE.FD_FIELDKEY_31,
                   FDFILE.FD_FIELDNO_32,
                   FDFILE.FD_FIELDROW_32,
                   FDFILE.FD_FIELDCOL_32,
                   FDFILE.FD_FIELDED_32,
                   FDFILE.FD_FIELDKEY_32,
                   FDFILE.FD_FIELDNO_33,
                   FDFILE.FD_FIELDROW_33,
                   FDFILE.FD_FIELDCOL_33,
                   FDFILE.FD_FIELDED_33,
                   FDFILE.FD_FIELDKEY_33,
                   FDFILE.FD_FIELDNO_34,
                   FDFILE.FD_FIELDROW_34,
                   FDFILE.FD_FIELDCOL_34,
                   FDFILE.FD_FIELDED_34,
                   FDFILE.FD_FIELDKEY_34,
                   FDFILE.FD_FIELDNO_35,
                   FDFILE.FD_FIELDROW_35,
                   FDFILE.FD_FIELDCOL_35,
                   FDFILE.FD_FIELDED_35,
                   FDFILE.FD_FIELDKEY_35,
                   FDFILE.FD_FIELDNO_36,
                   FDFILE.FD_FIELDROW_36,
                   FDFILE.FD_FIELDCOL_36,
                   FDFILE.FD_FIELDED_36,
                   FDFILE.FD_FIELDKEY_36,
                   FDFILE.FD_FIELDNO_37,
                   FDFILE.FD_FIELDROW_37,
                   FDFILE.FD_FIELDCOL_37,
                   FDFILE.FD_FIELDED_37,
                   FDFILE.FD_FIELDKEY_37,
                   FDFILE.FD_FIELDNO_38,
                   FDFILE.FD_FIELDROW_38,
                   FDFILE.FD_FIELDCOL_38,
                   FDFILE.FD_FIELDED_38,
                   FDFILE.FD_FIELDKEY_38,
                   FDFILE.FD_FIELDNO_39,
                   FDFILE.FD_FIELDROW_39,
                   FDFILE.FD_FIELDCOL_39,
                   FDFILE.FD_FIELDED_39,
                   FDFILE.FD_FIELDKEY_39,
                   FDFILE.FD_FIELDNO_40,
                   FDFILE.FD_FIELDROW_40,
                   FDFILE.FD_FIELDCOL_40,
                   FDFILE.FD_FIELDED_40,
                   FDFILE.FD_FIELDKEY_40,
                   FDFILE.FD_FIELDNO_41,
                   FDFILE.FD_FIELDROW_41,
                   FDFILE.FD_FIELDCOL_41,
                   FDFILE.FD_FIELDED_41,
                   FDFILE.FD_FIELDKEY_41,
                   FDFILE.FD_FIELDNO_42,
                   FDFILE.FD_FIELDROW_42,
                   FDFILE.FD_FIELDCOL_42,
                   FDFILE.FD_FIELDED_42,
                   FDFILE.FD_FIELDKEY_42,
                   FDFILE.FD_FIELDNO_43,
                   FDFILE.FD_FIELDROW_43,
                   FDFILE.FD_FIELDCOL_43,
                   FDFILE.FD_FIELDED_43,
                   FDFILE.FD_FIELDKEY_43,
                   FDFILE.FD_FIELDNO_44,
                   FDFILE.FD_FIELDROW_44,
                   FDFILE.FD_FIELDCOL_44,
                   FDFILE.FD_FIELDED_44,
                   FDFILE.FD_FIELDKEY_44,
                   FDFILE.FD_FIELDNO_45,
                   FDFILE.FD_FIELDROW_45,
                   FDFILE.FD_FIELDCOL_45,
                   FDFILE.FD_FIELDED_45,
                   FDFILE.FD_FIELDKEY_45,
                   FDFILE.FD_FIELDNO_46,
                   FDFILE.FD_FIELDROW_46,
                   FDFILE.FD_FIELDCOL_46,
                   FDFILE.FD_FIELDED_46,
                   FDFILE.FD_FIELDKEY_46,
                   FDFILE.FD_FIELDNO_47,
                   FDFILE.FD_FIELDROW_47,
                   FDFILE.FD_FIELDCOL_47,
                   FDFILE.FD_FIELDED_47,
                   FDFILE.FD_FIELDKEY_47,
                   FDFILE.FD_FIELDNO_48,
                   FDFILE.FD_FIELDROW_48,
                   FDFILE.FD_FIELDCOL_48,
                   FDFILE.FD_FIELDED_48,
                   FDFILE.FD_FIELDKEY_48,
                   FDFILE.FD_FIELDNO_49,
                   FDFILE.FD_FIELDROW_49,
                   FDFILE.FD_FIELDCOL_49,
                   FDFILE.FD_FIELDED_49,
                   FDFILE.FD_FIELDKEY_49,
                   FDFILE.FD_FIELDNO_50,
                   FDFILE.FD_FIELDROW_50,
                   FDFILE.FD_FIELDCOL_50,
                   FDFILE.FD_FIELDED_50,
                   FDFILE.FD_FIELDKEY_50,
                   FDFILE.FD_1STFILE,
                   ISNULL(FDFILE.FD_INCR_DELCNT,''),
                   ISNULL(FDFILE.FD_FMTYPE,''),
                   ISNULL(FDFILE.FD_PRCONT,''),
                   FDFILE.FD_NEXTFM,
                   ISNULL(FDFILE.FD_VALID_BRANCH,''),
                   FDFILE.FD_MIN_FORM_SENT_DAYS,
                   ISNULL(FDFILE.FD_SKIP_BAD_ADR,''),
                   ISNULL(FDFILE.FD_SECURITY_PROFILE,''),
                   ISNULL(FDFILE.FD_FORM_RIGHT_TO_CURE,''),
                   ISNULL(FDFILE.FD_TEST_WHICH_STATE,''),
                   ISNULL(FDFILE.FD_STATE_1,''),
                   ISNULL(FDFILE.FD_STATE_2,''),
                   ISNULL(FDFILE.FD_STATE_3,''),
                   ISNULL(FDFILE.FD_STATE_4,''),
                   ISNULL(FDFILE.FD_STATE_5,''),
                   ISNULL(FDFILE.FD_STATE_6,''),
                   ISNULL(FDFILE.FD_STATE_7,''),
                   ISNULL(FDFILE.FD_STATE_8,''),
                   ISNULL(FDFILE.FD_STATE_9,''),
                   ISNULL(FDFILE.FD_STATE_10,''),
                   ISNULL(FDFILE.FD_STATE_11,''),
                   ISNULL(FDFILE.FD_STATE_12,''),
                   ISNULL(FDFILE.FD_STATE_13,''),
                   ISNULL(FDFILE.FD_STATE_14,''),
                   ISNULL(FDFILE.FD_STATE_15,''),
                   ISNULL(FDFILE.FD_STATE_16,''),
                   ISNULL(FDFILE.FD_STATE_17,''),
                   ISNULL(FDFILE.FD_STATE_18,''),
                   ISNULL(FDFILE.FD_STATE_19,''),
                   ISNULL(FDFILE.FD_STATE_20,''),
                   ISNULL(FDFILE.FD_STATE_21,''),
                   ISNULL(FDFILE.FD_STATE_22,''),
                   ISNULL(FDFILE.FD_STATE_23,''),
                   ISNULL(FDFILE.FD_STATE_24,''),
                   ISNULL(FDFILE.FD_STATE_25,''),
                   ISNULL(FDFILE.FD_STATE_26,''),
                   ISNULL(FDFILE.FD_STATE_27,''),
                   ISNULL(FDFILE.FD_STATE_28,''),
                   ISNULL(FDFILE.FD_STATE_29,''),
                   ISNULL(FDFILE.FD_STATE_30,''),
                   ISNULL(FDFILE.FD_STATE_31,''),
                   ISNULL(FDFILE.FD_STATE_32,''),
                   ISNULL(FDFILE.FD_STATE_33,''),
                   ISNULL(FDFILE.FD_STATE_34,''),
                   ISNULL(FDFILE.FD_STATE_35,''),
                   ISNULL(FDFILE.FD_STATE_36,''),
                   ISNULL(FDFILE.FD_STATE_37,''),
                   ISNULL(FDFILE.FD_STATE_38,''),
                   ISNULL(FDFILE.FD_STATE_39,''),
                   ISNULL(FDFILE.FD_STATE_40,''),
                   ISNULL(FDFILE.FD_STATE_41,''),
                   ISNULL(FDFILE.FD_STATE_42,''),
                   ISNULL(FDFILE.FD_STATE_43,''),
                   ISNULL(FDFILE.FD_STATE_44,''),
                   ISNULL(FDFILE.FD_STATE_45,''),
                   ISNULL(FDFILE.FD_STATE_46,''),
                   ISNULL(FDFILE.FD_STATE_47,''),
                   ISNULL(FDFILE.FD_STATE_48,''),
                   ISNULL(FDFILE.FD_STATE_49,''),
                   ISNULL(FDFILE.FD_STATE_50,'')
            INTO :QFD-REC
            FROM DBO.FDFILE
            WHERE FDFILE.FD_FMNO = :QFD-FMNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-FD-FIELDS.

      *-----------------------------------------------------------------
       READ-FD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE FD-SELECT-PATH TO E-FILE.
           MOVE FD1-KEY TO E-KEYX.
           INITIALIZE QFD-REC.

           IF ( FD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH FD1_CURSOR INTO :QFD-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-FD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-FD1-FILE.
           IF ( FD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE FD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO FD1-CURSOR-STAT.

      *-----------------------------------------------------------------
      *    LOAD FD-REC FROM QFD-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-FD-FIELDS.
           MOVE QFD-FMNO TO FD-FMNO.

           IF ( QFD-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO FD-LTOUCH-DATE
           ELSE
              MOVE QFD-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO FD-LTOUCH-DATE.

           MOVE QFD-DESC TO FD-DESC.
           MOVE QFD-OPTION TO FD-OPTION.
           MOVE QFD-FORM-CHG-CD TO FD-FORM-CHG-CD.
           MOVE QFD-PROG-PREFIX TO FD-PROG-PREFIX.
           MOVE QFD-TYPE TO FD-TYPE.
           MOVE QFD-LNMEMO TO FD-LNMEMO.
           MOVE QFD-BODY TO FD-BODY.
           MOVE QFD-TOP TO FD-TOP.
           MOVE QFD-BOTTOM TO FD-BOTTOM.
           MOVE QFD-PROG TO FD-PROG.
           MOVE QFD-PROMPTS-1 TO FD-PROMPTS(1).
           MOVE QFD-PROMPTS-2 TO FD-PROMPTS(2).
           MOVE QFD-PROMPTS-3 TO FD-PROMPTS(3).
           MOVE QFD-PROMPTS-4 TO FD-PROMPTS(4).
           MOVE QFD-PROMPTS-5 TO FD-PROMPTS(5).
           MOVE QFD-PROMPTS-6 TO FD-PROMPTS(6).
           MOVE QFD-PROMPTS-7 TO FD-PROMPTS(7).
           MOVE QFD-PROMPTS-8 TO FD-PROMPTS(8).
           MOVE QFD-PROMPTS-9 TO FD-PROMPTS(9).
           MOVE QFD-PROMPTS-10 TO FD-PROMPTS(10).
           MOVE QFD-PROMPTS-11 TO FD-PROMPTS(11).
           MOVE QFD-PROMPTS-12 TO FD-PROMPTS(12).
           MOVE QFD-PROMPTS-13 TO FD-PROMPTS(13).
           MOVE QFD-PROMPTS-14 TO FD-PROMPTS(14).
           MOVE QFD-PROMPTS-15 TO FD-PROMPTS(15).
           MOVE QFD-PROMPTS-16 TO FD-PROMPTS(16).
           MOVE QFD-PROMPTS-17 TO FD-PROMPTS(17).
           MOVE QFD-PROMPTS-18 TO FD-PROMPTS(18).
           MOVE QFD-PROMPTS-19 TO FD-PROMPTS(19).
           MOVE QFD-PROMPTS-20 TO FD-PROMPTS(20).
           MOVE QFD-PROMPTS-21 TO FD-PROMPTS(21).
           MOVE QFD-PROMPTS-22 TO FD-PROMPTS(22).
           MOVE QFD-PROMPTS-23 TO FD-PROMPTS(23).
           MOVE QFD-PROMPTS-24 TO FD-PROMPTS(24).
           MOVE QFD-PROMPTS-25 TO FD-PROMPTS(25).
           MOVE QFD-PROMPTS-26 TO FD-PROMPTS(26).
           MOVE QFD-PROMPTS-27 TO FD-PROMPTS(27).
           MOVE QFD-PROMPTS-28 TO FD-PROMPTS(28).
           MOVE QFD-PROMPTS-29 TO FD-PROMPTS(29).
           MOVE QFD-PROMPTS-30 TO FD-PROMPTS(30).
           MOVE QFD-FIELDNO-1 TO FD-FIELDNO(1).
           MOVE QFD-FIELDROW-1 TO FD-FIELDROW(1).
           MOVE QFD-FIELDCOL-1 TO FD-FIELDCOL(1).
           MOVE QFD-FIELDED-1 TO FD-FIELDED(1).
           MOVE QFD-FIELDKEY-1 TO FD-FIELDKEY(1).
           MOVE QFD-FIELDNO-2 TO FD-FIELDNO(2).
           MOVE QFD-FIELDROW-2 TO FD-FIELDROW(2).
           MOVE QFD-FIELDCOL-2 TO FD-FIELDCOL(2).
           MOVE QFD-FIELDED-2 TO FD-FIELDED(2).
           MOVE QFD-FIELDKEY-2 TO FD-FIELDKEY(2).
           MOVE QFD-FIELDNO-3 TO FD-FIELDNO(3).
           MOVE QFD-FIELDROW-3 TO FD-FIELDROW(3).
           MOVE QFD-FIELDCOL-3 TO FD-FIELDCOL(3).
           MOVE QFD-FIELDED-3 TO FD-FIELDED(3).
           MOVE QFD-FIELDKEY-3 TO FD-FIELDKEY(3).
           MOVE QFD-FIELDNO-4 TO FD-FIELDNO(4).
           MOVE QFD-FIELDROW-4 TO FD-FIELDROW(4).
           MOVE QFD-FIELDCOL-4 TO FD-FIELDCOL(4).
           MOVE QFD-FIELDED-4 TO FD-FIELDED(4).
           MOVE QFD-FIELDKEY-4 TO FD-FIELDKEY(4).
           MOVE QFD-FIELDNO-5 TO FD-FIELDNO(5).
           MOVE QFD-FIELDROW-5 TO FD-FIELDROW(5).
           MOVE QFD-FIELDCOL-5 TO FD-FIELDCOL(5).
           MOVE QFD-FIELDED-5 TO FD-FIELDED(5).
           MOVE QFD-FIELDKEY-5 TO FD-FIELDKEY(5).
           MOVE QFD-FIELDNO-6 TO FD-FIELDNO(6).
           MOVE QFD-FIELDROW-6 TO FD-FIELDROW(6).
           MOVE QFD-FIELDCOL-6 TO FD-FIELDCOL(6).
           MOVE QFD-FIELDED-6 TO FD-FIELDED(6).
           MOVE QFD-FIELDKEY-6 TO FD-FIELDKEY(6).
           MOVE QFD-FIELDNO-7 TO FD-FIELDNO(7).
           MOVE QFD-FIELDROW-7 TO FD-FIELDROW(7).
           MOVE QFD-FIELDCOL-7 TO FD-FIELDCOL(7).
           MOVE QFD-FIELDED-7 TO FD-FIELDED(7).
           MOVE QFD-FIELDKEY-7 TO FD-FIELDKEY(7).
           MOVE QFD-FIELDNO-8 TO FD-FIELDNO(8).
           MOVE QFD-FIELDROW-8 TO FD-FIELDROW(8).
           MOVE QFD-FIELDCOL-8 TO FD-FIELDCOL(8).
           MOVE QFD-FIELDED-8 TO FD-FIELDED(8).
           MOVE QFD-FIELDKEY-8 TO FD-FIELDKEY(8).
           MOVE QFD-FIELDNO-9 TO FD-FIELDNO(9).
           MOVE QFD-FIELDROW-9 TO FD-FIELDROW(9).
           MOVE QFD-FIELDCOL-9 TO FD-FIELDCOL(9).
           MOVE QFD-FIELDED-9 TO FD-FIELDED(9).
           MOVE QFD-FIELDKEY-9 TO FD-FIELDKEY(9).
           MOVE QFD-FIELDNO-10 TO FD-FIELDNO(10).
           MOVE QFD-FIELDROW-10 TO FD-FIELDROW(10).
           MOVE QFD-FIELDCOL-10 TO FD-FIELDCOL(10).
           MOVE QFD-FIELDED-10 TO FD-FIELDED(10).
           MOVE QFD-FIELDKEY-10 TO FD-FIELDKEY(10).
           MOVE QFD-FIELDNO-11 TO FD-FIELDNO(11).
           MOVE QFD-FIELDROW-11 TO FD-FIELDROW(11).
           MOVE QFD-FIELDCOL-11 TO FD-FIELDCOL(11).
           MOVE QFD-FIELDED-11 TO FD-FIELDED(11).
           MOVE QFD-FIELDKEY-11 TO FD-FIELDKEY(11).
           MOVE QFD-FIELDNO-12 TO FD-FIELDNO(12).
           MOVE QFD-FIELDROW-12 TO FD-FIELDROW(12).
           MOVE QFD-FIELDCOL-12 TO FD-FIELDCOL(12).
           MOVE QFD-FIELDED-12 TO FD-FIELDED(12).
           MOVE QFD-FIELDKEY-12 TO FD-FIELDKEY(12).
           MOVE QFD-FIELDNO-13 TO FD-FIELDNO(13).
           MOVE QFD-FIELDROW-13 TO FD-FIELDROW(13).
           MOVE QFD-FIELDCOL-13 TO FD-FIELDCOL(13).
           MOVE QFD-FIELDED-13 TO FD-FIELDED(13).
           MOVE QFD-FIELDKEY-13 TO FD-FIELDKEY(13).
           MOVE QFD-FIELDNO-14 TO FD-FIELDNO(14).
           MOVE QFD-FIELDROW-14 TO FD-FIELDROW(14).
           MOVE QFD-FIELDCOL-14 TO FD-FIELDCOL(14).
           MOVE QFD-FIELDED-14 TO FD-FIELDED(14).
           MOVE QFD-FIELDKEY-14 TO FD-FIELDKEY(14).
           MOVE QFD-FIELDNO-15 TO FD-FIELDNO(15).
           MOVE QFD-FIELDROW-15 TO FD-FIELDROW(15).
           MOVE QFD-FIELDCOL-15 TO FD-FIELDCOL(15).
           MOVE QFD-FIELDED-15 TO FD-FIELDED(15).
           MOVE QFD-FIELDKEY-15 TO FD-FIELDKEY(15).
           MOVE QFD-FIELDNO-16 TO FD-FIELDNO(16).
           MOVE QFD-FIELDROW-16 TO FD-FIELDROW(16).
           MOVE QFD-FIELDCOL-16 TO FD-FIELDCOL(16).
           MOVE QFD-FIELDED-16 TO FD-FIELDED(16).
           MOVE QFD-FIELDKEY-16 TO FD-FIELDKEY(16).
           MOVE QFD-FIELDNO-17 TO FD-FIELDNO(17).
           MOVE QFD-FIELDROW-17 TO FD-FIELDROW(17).
           MOVE QFD-FIELDCOL-17 TO FD-FIELDCOL(17).
           MOVE QFD-FIELDED-17 TO FD-FIELDED(17).
           MOVE QFD-FIELDKEY-17 TO FD-FIELDKEY(17).
           MOVE QFD-FIELDNO-18 TO FD-FIELDNO(18).
           MOVE QFD-FIELDROW-18 TO FD-FIELDROW(18).
           MOVE QFD-FIELDCOL-18 TO FD-FIELDCOL(18).
           MOVE QFD-FIELDED-18 TO FD-FIELDED(18).
           MOVE QFD-FIELDKEY-18 TO FD-FIELDKEY(18).
           MOVE QFD-FIELDNO-19 TO FD-FIELDNO(19).
           MOVE QFD-FIELDROW-19 TO FD-FIELDROW(19).
           MOVE QFD-FIELDCOL-19 TO FD-FIELDCOL(19).
           MOVE QFD-FIELDED-19 TO FD-FIELDED(19).
           MOVE QFD-FIELDKEY-19 TO FD-FIELDKEY(19).
           MOVE QFD-FIELDNO-20 TO FD-FIELDNO(20).
           MOVE QFD-FIELDROW-20 TO FD-FIELDROW(20).
           MOVE QFD-FIELDCOL-20 TO FD-FIELDCOL(20).
           MOVE QFD-FIELDED-20 TO FD-FIELDED(20).
           MOVE QFD-FIELDKEY-20 TO FD-FIELDKEY(20).
           MOVE QFD-FIELDNO-21 TO FD-FIELDNO(21).
           MOVE QFD-FIELDROW-21 TO FD-FIELDROW(21).
           MOVE QFD-FIELDCOL-21 TO FD-FIELDCOL(21).
           MOVE QFD-FIELDED-21 TO FD-FIELDED(21).
           MOVE QFD-FIELDKEY-21 TO FD-FIELDKEY(21).
           MOVE QFD-FIELDNO-22 TO FD-FIELDNO(22).
           MOVE QFD-FIELDROW-22 TO FD-FIELDROW(22).
           MOVE QFD-FIELDCOL-22 TO FD-FIELDCOL(22).
           MOVE QFD-FIELDED-22 TO FD-FIELDED(22).
           MOVE QFD-FIELDKEY-22 TO FD-FIELDKEY(22).
           MOVE QFD-FIELDNO-23 TO FD-FIELDNO(23).
           MOVE QFD-FIELDROW-23 TO FD-FIELDROW(23).
           MOVE QFD-FIELDCOL-23 TO FD-FIELDCOL(23).
           MOVE QFD-FIELDED-23 TO FD-FIELDED(23).
           MOVE QFD-FIELDKEY-23 TO FD-FIELDKEY(23).
           MOVE QFD-FIELDNO-24 TO FD-FIELDNO(24).
           MOVE QFD-FIELDROW-24 TO FD-FIELDROW(24).
           MOVE QFD-FIELDCOL-24 TO FD-FIELDCOL(24).
           MOVE QFD-FIELDED-24 TO FD-FIELDED(24).
           MOVE QFD-FIELDKEY-24 TO FD-FIELDKEY(24).
           MOVE QFD-FIELDNO-25 TO FD-FIELDNO(25).
           MOVE QFD-FIELDROW-25 TO FD-FIELDROW(25).
           MOVE QFD-FIELDCOL-25 TO FD-FIELDCOL(25).
           MOVE QFD-FIELDED-25 TO FD-FIELDED(25).
           MOVE QFD-FIELDKEY-25 TO FD-FIELDKEY(25).
           MOVE QFD-FIELDNO-26 TO FD-FIELDNO(26).
           MOVE QFD-FIELDROW-26 TO FD-FIELDROW(26).
           MOVE QFD-FIELDCOL-26 TO FD-FIELDCOL(26).
           MOVE QFD-FIELDED-26 TO FD-FIELDED(26).
           MOVE QFD-FIELDKEY-26 TO FD-FIELDKEY(26).
           MOVE QFD-FIELDNO-27 TO FD-FIELDNO(27).
           MOVE QFD-FIELDROW-27 TO FD-FIELDROW(27).
           MOVE QFD-FIELDCOL-27 TO FD-FIELDCOL(27).
           MOVE QFD-FIELDED-27 TO FD-FIELDED(27).
           MOVE QFD-FIELDKEY-27 TO FD-FIELDKEY(27).
           MOVE QFD-FIELDNO-28 TO FD-FIELDNO(28).
           MOVE QFD-FIELDROW-28 TO FD-FIELDROW(28).
           MOVE QFD-FIELDCOL-28 TO FD-FIELDCOL(28).
           MOVE QFD-FIELDED-28 TO FD-FIELDED(28).
           MOVE QFD-FIELDKEY-28 TO FD-FIELDKEY(28).
           MOVE QFD-FIELDNO-29 TO FD-FIELDNO(29).
           MOVE QFD-FIELDROW-29 TO FD-FIELDROW(29).
           MOVE QFD-FIELDCOL-29 TO FD-FIELDCOL(29).
           MOVE QFD-FIELDED-29 TO FD-FIELDED(29).
           MOVE QFD-FIELDKEY-29 TO FD-FIELDKEY(29).
           MOVE QFD-FIELDNO-30 TO FD-FIELDNO(30).
           MOVE QFD-FIELDROW-30 TO FD-FIELDROW(30).
           MOVE QFD-FIELDCOL-30 TO FD-FIELDCOL(30).
           MOVE QFD-FIELDED-30 TO FD-FIELDED(30).
           MOVE QFD-FIELDKEY-30 TO FD-FIELDKEY(30).
           MOVE QFD-FIELDNO-31 TO FD-FIELDNO(31).
           MOVE QFD-FIELDROW-31 TO FD-FIELDROW(31).
           MOVE QFD-FIELDCOL-31 TO FD-FIELDCOL(31).
           MOVE QFD-FIELDED-31 TO FD-FIELDED(31).
           MOVE QFD-FIELDKEY-31 TO FD-FIELDKEY(31).
           MOVE QFD-FIELDNO-32 TO FD-FIELDNO(32).
           MOVE QFD-FIELDROW-32 TO FD-FIELDROW(32).
           MOVE QFD-FIELDCOL-32 TO FD-FIELDCOL(32).
           MOVE QFD-FIELDED-32 TO FD-FIELDED(32).
           MOVE QFD-FIELDKEY-32 TO FD-FIELDKEY(32).
           MOVE QFD-FIELDNO-33 TO FD-FIELDNO(33).
           MOVE QFD-FIELDROW-33 TO FD-FIELDROW(33).
           MOVE QFD-FIELDCOL-33 TO FD-FIELDCOL(33).
           MOVE QFD-FIELDED-33 TO FD-FIELDED(33).
           MOVE QFD-FIELDKEY-33 TO FD-FIELDKEY(33).
           MOVE QFD-FIELDNO-34 TO FD-FIELDNO(34).
           MOVE QFD-FIELDROW-34 TO FD-FIELDROW(34).
           MOVE QFD-FIELDCOL-34 TO FD-FIELDCOL(34).
           MOVE QFD-FIELDED-34 TO FD-FIELDED(34).
           MOVE QFD-FIELDKEY-34 TO FD-FIELDKEY(34).
           MOVE QFD-FIELDNO-35 TO FD-FIELDNO(35).
           MOVE QFD-FIELDROW-35 TO FD-FIELDROW(35).
           MOVE QFD-FIELDCOL-35 TO FD-FIELDCOL(35).
           MOVE QFD-FIELDED-35 TO FD-FIELDED(35).
           MOVE QFD-FIELDKEY-35 TO FD-FIELDKEY(35).
           MOVE QFD-FIELDNO-36 TO FD-FIELDNO(36).
           MOVE QFD-FIELDROW-36 TO FD-FIELDROW(36).
           MOVE QFD-FIELDCOL-36 TO FD-FIELDCOL(36).
           MOVE QFD-FIELDED-36 TO FD-FIELDED(36).
           MOVE QFD-FIELDKEY-36 TO FD-FIELDKEY(36).
           MOVE QFD-FIELDNO-37 TO FD-FIELDNO(37).
           MOVE QFD-FIELDROW-37 TO FD-FIELDROW(37).
           MOVE QFD-FIELDCOL-37 TO FD-FIELDCOL(37).
           MOVE QFD-FIELDED-37 TO FD-FIELDED(37).
           MOVE QFD-FIELDKEY-37 TO FD-FIELDKEY(37).
           MOVE QFD-FIELDNO-38 TO FD-FIELDNO(38).
           MOVE QFD-FIELDROW-38 TO FD-FIELDROW(38).
           MOVE QFD-FIELDCOL-38 TO FD-FIELDCOL(38).
           MOVE QFD-FIELDED-38 TO FD-FIELDED(38).
           MOVE QFD-FIELDKEY-38 TO FD-FIELDKEY(38).
           MOVE QFD-FIELDNO-39 TO FD-FIELDNO(39).
           MOVE QFD-FIELDROW-39 TO FD-FIELDROW(39).
           MOVE QFD-FIELDCOL-39 TO FD-FIELDCOL(39).
           MOVE QFD-FIELDED-39 TO FD-FIELDED(39).
           MOVE QFD-FIELDKEY-39 TO FD-FIELDKEY(39).
           MOVE QFD-FIELDNO-40 TO FD-FIELDNO(40).
           MOVE QFD-FIELDROW-40 TO FD-FIELDROW(40).
           MOVE QFD-FIELDCOL-40 TO FD-FIELDCOL(40).
           MOVE QFD-FIELDED-40 TO FD-FIELDED(40).
           MOVE QFD-FIELDKEY-40 TO FD-FIELDKEY(40).
           MOVE QFD-FIELDNO-41 TO FD-FIELDNO(41).
           MOVE QFD-FIELDROW-41 TO FD-FIELDROW(41).
           MOVE QFD-FIELDCOL-41 TO FD-FIELDCOL(41).
           MOVE QFD-FIELDED-41 TO FD-FIELDED(41).
           MOVE QFD-FIELDKEY-41 TO FD-FIELDKEY(41).
           MOVE QFD-FIELDNO-42 TO FD-FIELDNO(42).
           MOVE QFD-FIELDROW-42 TO FD-FIELDROW(42).
           MOVE QFD-FIELDCOL-42 TO FD-FIELDCOL(42).
           MOVE QFD-FIELDED-42 TO FD-FIELDED(42).
           MOVE QFD-FIELDKEY-42 TO FD-FIELDKEY(42).
           MOVE QFD-FIELDNO-43 TO FD-FIELDNO(43).
           MOVE QFD-FIELDROW-43 TO FD-FIELDROW(43).
           MOVE QFD-FIELDCOL-43 TO FD-FIELDCOL(43).
           MOVE QFD-FIELDED-43 TO FD-FIELDED(43).
           MOVE QFD-FIELDKEY-43 TO FD-FIELDKEY(43).
           MOVE QFD-FIELDNO-44 TO FD-FIELDNO(44).
           MOVE QFD-FIELDROW-44 TO FD-FIELDROW(44).
           MOVE QFD-FIELDCOL-44 TO FD-FIELDCOL(44).
           MOVE QFD-FIELDED-44 TO FD-FIELDED(44).
           MOVE QFD-FIELDKEY-44 TO FD-FIELDKEY(44).
           MOVE QFD-FIELDNO-45 TO FD-FIELDNO(45).
           MOVE QFD-FIELDROW-45 TO FD-FIELDROW(45).
           MOVE QFD-FIELDCOL-45 TO FD-FIELDCOL(45).
           MOVE QFD-FIELDED-45 TO FD-FIELDED(45).
           MOVE QFD-FIELDKEY-45 TO FD-FIELDKEY(45).
           MOVE QFD-FIELDNO-46 TO FD-FIELDNO(46).
           MOVE QFD-FIELDROW-46 TO FD-FIELDROW(46).
           MOVE QFD-FIELDCOL-46 TO FD-FIELDCOL(46).
           MOVE QFD-FIELDED-46 TO FD-FIELDED(46).
           MOVE QFD-FIELDKEY-46 TO FD-FIELDKEY(46).
           MOVE QFD-FIELDNO-47 TO FD-FIELDNO(47).
           MOVE QFD-FIELDROW-47 TO FD-FIELDROW(47).
           MOVE QFD-FIELDCOL-47 TO FD-FIELDCOL(47).
           MOVE QFD-FIELDED-47 TO FD-FIELDED(47).
           MOVE QFD-FIELDKEY-47 TO FD-FIELDKEY(47).
           MOVE QFD-FIELDNO-48 TO FD-FIELDNO(48).
           MOVE QFD-FIELDROW-48 TO FD-FIELDROW(48).
           MOVE QFD-FIELDCOL-48 TO FD-FIELDCOL(48).
           MOVE QFD-FIELDED-48 TO FD-FIELDED(48).
           MOVE QFD-FIELDKEY-48 TO FD-FIELDKEY(48).
           MOVE QFD-FIELDNO-49 TO FD-FIELDNO(49).
           MOVE QFD-FIELDROW-49 TO FD-FIELDROW(49).
           MOVE QFD-FIELDCOL-49 TO FD-FIELDCOL(49).
           MOVE QFD-FIELDED-49 TO FD-FIELDED(49).
           MOVE QFD-FIELDKEY-49 TO FD-FIELDKEY(49).
           MOVE QFD-FIELDNO-50 TO FD-FIELDNO(50).
           MOVE QFD-FIELDROW-50 TO FD-FIELDROW(50).
           MOVE QFD-FIELDCOL-50 TO FD-FIELDCOL(50).
           MOVE QFD-FIELDED-50 TO FD-FIELDED(50).
           MOVE QFD-FIELDKEY-50 TO FD-FIELDKEY(50).
           MOVE QFD-1STFILE TO FD-1STFILE.
           MOVE QFD-INCR-DELCNT TO FD-INCR-DELCNT.
           MOVE QFD-FMTYPE TO FD-FMTYPE.
           MOVE QFD-PRCONT TO FD-PRCONT.
           MOVE QFD-NEXTFM TO FD-NEXTFM.
           MOVE QFD-VALID-BRANCH TO FD-VALID-BRANCH.
           MOVE QFD-MIN-FORM-SENT-DAYS TO FD-MIN-FORM-SENT-DAYS.
           MOVE QFD-SKIP-BAD-ADR TO FD-SKIP-BAD-ADR.
           MOVE QFD-SECURITY-PROFILE TO FD-SECURITY-PROFILE.
           MOVE QFD-FORM-RIGHT-TO-CURE TO FD-FORM-RIGHT-TO-CURE.
           MOVE QFD-TEST-WHICH-STATE TO FD-TEST-WHICH-STATE.
           MOVE QFD-STATE-1 TO FD-STATE(1).
           MOVE QFD-STATE-2 TO FD-STATE(2).
           MOVE QFD-STATE-3 TO FD-STATE(3).
           MOVE QFD-STATE-4 TO FD-STATE(4).
           MOVE QFD-STATE-5 TO FD-STATE(5).
           MOVE QFD-STATE-6 TO FD-STATE(6).
           MOVE QFD-STATE-7 TO FD-STATE(7).
           MOVE QFD-STATE-8 TO FD-STATE(8).
           MOVE QFD-STATE-9 TO FD-STATE(9).
           MOVE QFD-STATE-10 TO FD-STATE(10).
           MOVE QFD-STATE-11 TO FD-STATE(11).
           MOVE QFD-STATE-12 TO FD-STATE(12).
           MOVE QFD-STATE-13 TO FD-STATE(13).
           MOVE QFD-STATE-14 TO FD-STATE(14).
           MOVE QFD-STATE-15 TO FD-STATE(15).
           MOVE QFD-STATE-16 TO FD-STATE(16).
           MOVE QFD-STATE-17 TO FD-STATE(17).
           MOVE QFD-STATE-18 TO FD-STATE(18).
           MOVE QFD-STATE-19 TO FD-STATE(19).
           MOVE QFD-STATE-20 TO FD-STATE(20).
           MOVE QFD-STATE-21 TO FD-STATE(21).
           MOVE QFD-STATE-22 TO FD-STATE(22).
           MOVE QFD-STATE-23 TO FD-STATE(23).
           MOVE QFD-STATE-24 TO FD-STATE(24).
           MOVE QFD-STATE-25 TO FD-STATE(25).
           MOVE QFD-STATE-26 TO FD-STATE(26).
           MOVE QFD-STATE-27 TO FD-STATE(27).
           MOVE QFD-STATE-28 TO FD-STATE(28).
           MOVE QFD-STATE-29 TO FD-STATE(29).
           MOVE QFD-STATE-30 TO FD-STATE(30).
           MOVE QFD-STATE-31 TO FD-STATE(31).
           MOVE QFD-STATE-32 TO FD-STATE(32).
           MOVE QFD-STATE-33 TO FD-STATE(33).
           MOVE QFD-STATE-34 TO FD-STATE(34).
           MOVE QFD-STATE-35 TO FD-STATE(35).
           MOVE QFD-STATE-36 TO FD-STATE(36).
           MOVE QFD-STATE-37 TO FD-STATE(37).
           MOVE QFD-STATE-38 TO FD-STATE(38).
           MOVE QFD-STATE-39 TO FD-STATE(39).
           MOVE QFD-STATE-40 TO FD-STATE(40).
           MOVE QFD-STATE-41 TO FD-STATE(41).
           MOVE QFD-STATE-42 TO FD-STATE(42).
           MOVE QFD-STATE-43 TO FD-STATE(43).
           MOVE QFD-STATE-44 TO FD-STATE(44).
           MOVE QFD-STATE-45 TO FD-STATE(45).
           MOVE QFD-STATE-46 TO FD-STATE(46).
           MOVE QFD-STATE-47 TO FD-STATE(47).
           MOVE QFD-STATE-48 TO FD-STATE(48).
           MOVE QFD-STATE-49 TO FD-STATE(49).
           MOVE QFD-STATE-50 TO FD-STATE(50).
 
      *-----------------------------------------------------------------
      *    LOAD QFD-REC FROM FD-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-FD-FIELDS.
           MOVE FD-FMNO TO QFD-FMNO.

           IF ( FD-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QFD-LTOUCH-DATE
           ELSE
              MOVE FD-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QFD-LTOUCH-DATE.

           MOVE FD-DESC TO QFD-DESC.
           MOVE FD-OPTION TO QFD-OPTION.
           MOVE FD-FORM-CHG-CD TO QFD-FORM-CHG-CD.
           MOVE FD-PROG-PREFIX TO QFD-PROG-PREFIX.
           MOVE FD-TYPE TO QFD-TYPE.
           MOVE FD-LNMEMO TO QFD-LNMEMO.
           MOVE FD-BODY TO QFD-BODY.
           MOVE FD-TOP TO QFD-TOP.
           MOVE FD-BOTTOM TO QFD-BOTTOM.
           MOVE FD-PROG TO QFD-PROG.
           MOVE FD-PROMPTS(1) TO QFD-PROMPTS-1.
           MOVE FD-PROMPTS(2) TO QFD-PROMPTS-2.
           MOVE FD-PROMPTS(3) TO QFD-PROMPTS-3.
           MOVE FD-PROMPTS(4) TO QFD-PROMPTS-4.
           MOVE FD-PROMPTS(5) TO QFD-PROMPTS-5.
           MOVE FD-PROMPTS(6) TO QFD-PROMPTS-6.
           MOVE FD-PROMPTS(7) TO QFD-PROMPTS-7.
           MOVE FD-PROMPTS(8) TO QFD-PROMPTS-8.
           MOVE FD-PROMPTS(9) TO QFD-PROMPTS-9.
           MOVE FD-PROMPTS(10) TO QFD-PROMPTS-10.
           MOVE FD-PROMPTS(11) TO QFD-PROMPTS-11.
           MOVE FD-PROMPTS(12) TO QFD-PROMPTS-12.
           MOVE FD-PROMPTS(13) TO QFD-PROMPTS-13.
           MOVE FD-PROMPTS(14) TO QFD-PROMPTS-14.
           MOVE FD-PROMPTS(15) TO QFD-PROMPTS-15.
           MOVE FD-PROMPTS(16) TO QFD-PROMPTS-16.
           MOVE FD-PROMPTS(17) TO QFD-PROMPTS-17.
           MOVE FD-PROMPTS(18) TO QFD-PROMPTS-18.
           MOVE FD-PROMPTS(19) TO QFD-PROMPTS-19.
           MOVE FD-PROMPTS(20) TO QFD-PROMPTS-20.
           MOVE FD-PROMPTS(21) TO QFD-PROMPTS-21.
           MOVE FD-PROMPTS(22) TO QFD-PROMPTS-22.
           MOVE FD-PROMPTS(23) TO QFD-PROMPTS-23.
           MOVE FD-PROMPTS(24) TO QFD-PROMPTS-24.
           MOVE FD-PROMPTS(25) TO QFD-PROMPTS-25.
           MOVE FD-PROMPTS(26) TO QFD-PROMPTS-26.
           MOVE FD-PROMPTS(27) TO QFD-PROMPTS-27.
           MOVE FD-PROMPTS(28) TO QFD-PROMPTS-28.
           MOVE FD-PROMPTS(29) TO QFD-PROMPTS-29.
           MOVE FD-PROMPTS(30) TO QFD-PROMPTS-30.
           MOVE FD-FIELDNO(1) TO QFD-FIELDNO-1.
           MOVE FD-FIELDROW(1) TO QFD-FIELDROW-1.
           MOVE FD-FIELDCOL(1) TO QFD-FIELDCOL-1.
           MOVE FD-FIELDED(1) TO QFD-FIELDED-1.
           MOVE FD-FIELDKEY(1) TO QFD-FIELDKEY-1.
           MOVE FD-FIELDNO(2) TO QFD-FIELDNO-2.
           MOVE FD-FIELDROW(2) TO QFD-FIELDROW-2.
           MOVE FD-FIELDCOL(2) TO QFD-FIELDCOL-2.
           MOVE FD-FIELDED(2) TO QFD-FIELDED-2.
           MOVE FD-FIELDKEY(2) TO QFD-FIELDKEY-2.
           MOVE FD-FIELDNO(3) TO QFD-FIELDNO-3.
           MOVE FD-FIELDROW(3) TO QFD-FIELDROW-3.
           MOVE FD-FIELDCOL(3) TO QFD-FIELDCOL-3.
           MOVE FD-FIELDED(3) TO QFD-FIELDED-3.
           MOVE FD-FIELDKEY(3) TO QFD-FIELDKEY-3.
           MOVE FD-FIELDNO(4) TO QFD-FIELDNO-4.
           MOVE FD-FIELDROW(4) TO QFD-FIELDROW-4.
           MOVE FD-FIELDCOL(4) TO QFD-FIELDCOL-4.
           MOVE FD-FIELDED(4) TO QFD-FIELDED-4.
           MOVE FD-FIELDKEY(4) TO QFD-FIELDKEY-4.
           MOVE FD-FIELDNO(5) TO QFD-FIELDNO-5.
           MOVE FD-FIELDROW(5) TO QFD-FIELDROW-5.
           MOVE FD-FIELDCOL(5) TO QFD-FIELDCOL-5.
           MOVE FD-FIELDED(5) TO QFD-FIELDED-5.
           MOVE FD-FIELDKEY(5) TO QFD-FIELDKEY-5.
           MOVE FD-FIELDNO(6) TO QFD-FIELDNO-6.
           MOVE FD-FIELDROW(6) TO QFD-FIELDROW-6.
           MOVE FD-FIELDCOL(6) TO QFD-FIELDCOL-6.
           MOVE FD-FIELDED(6) TO QFD-FIELDED-6.
           MOVE FD-FIELDKEY(6) TO QFD-FIELDKEY-6.
           MOVE FD-FIELDNO(7) TO QFD-FIELDNO-7.
           MOVE FD-FIELDROW(7) TO QFD-FIELDROW-7.
           MOVE FD-FIELDCOL(7) TO QFD-FIELDCOL-7.
           MOVE FD-FIELDED(7) TO QFD-FIELDED-7.
           MOVE FD-FIELDKEY(7) TO QFD-FIELDKEY-7.
           MOVE FD-FIELDNO(8) TO QFD-FIELDNO-8.
           MOVE FD-FIELDROW(8) TO QFD-FIELDROW-8.
           MOVE FD-FIELDCOL(8) TO QFD-FIELDCOL-8.
           MOVE FD-FIELDED(8) TO QFD-FIELDED-8.
           MOVE FD-FIELDKEY(8) TO QFD-FIELDKEY-8.
           MOVE FD-FIELDNO(9) TO QFD-FIELDNO-9.
           MOVE FD-FIELDROW(9) TO QFD-FIELDROW-9.
           MOVE FD-FIELDCOL(9) TO QFD-FIELDCOL-9.
           MOVE FD-FIELDED(9) TO QFD-FIELDED-9.
           MOVE FD-FIELDKEY(9) TO QFD-FIELDKEY-9.
           MOVE FD-FIELDNO(10) TO QFD-FIELDNO-10.
           MOVE FD-FIELDROW(10) TO QFD-FIELDROW-10.
           MOVE FD-FIELDCOL(10) TO QFD-FIELDCOL-10.
           MOVE FD-FIELDED(10) TO QFD-FIELDED-10.
           MOVE FD-FIELDKEY(10) TO QFD-FIELDKEY-10.
           MOVE FD-FIELDNO(11) TO QFD-FIELDNO-11.
           MOVE FD-FIELDROW(11) TO QFD-FIELDROW-11.
           MOVE FD-FIELDCOL(11) TO QFD-FIELDCOL-11.
           MOVE FD-FIELDED(11) TO QFD-FIELDED-11.
           MOVE FD-FIELDKEY(11) TO QFD-FIELDKEY-11.
           MOVE FD-FIELDNO(12) TO QFD-FIELDNO-12.
           MOVE FD-FIELDROW(12) TO QFD-FIELDROW-12.
           MOVE FD-FIELDCOL(12) TO QFD-FIELDCOL-12.
           MOVE FD-FIELDED(12) TO QFD-FIELDED-12.
           MOVE FD-FIELDKEY(12) TO QFD-FIELDKEY-12.
           MOVE FD-FIELDNO(13) TO QFD-FIELDNO-13.
           MOVE FD-FIELDROW(13) TO QFD-FIELDROW-13.
           MOVE FD-FIELDCOL(13) TO QFD-FIELDCOL-13.
           MOVE FD-FIELDED(13) TO QFD-FIELDED-13.
           MOVE FD-FIELDKEY(13) TO QFD-FIELDKEY-13.
           MOVE FD-FIELDNO(14) TO QFD-FIELDNO-14.
           MOVE FD-FIELDROW(14) TO QFD-FIELDROW-14.
           MOVE FD-FIELDCOL(14) TO QFD-FIELDCOL-14.
           MOVE FD-FIELDED(14) TO QFD-FIELDED-14.
           MOVE FD-FIELDKEY(14) TO QFD-FIELDKEY-14.
           MOVE FD-FIELDNO(15) TO QFD-FIELDNO-15.
           MOVE FD-FIELDROW(15) TO QFD-FIELDROW-15.
           MOVE FD-FIELDCOL(15) TO QFD-FIELDCOL-15.
           MOVE FD-FIELDED(15) TO QFD-FIELDED-15.
           MOVE FD-FIELDKEY(15) TO QFD-FIELDKEY-15.
           MOVE FD-FIELDNO(16) TO QFD-FIELDNO-16.
           MOVE FD-FIELDROW(16) TO QFD-FIELDROW-16.
           MOVE FD-FIELDCOL(16) TO QFD-FIELDCOL-16.
           MOVE FD-FIELDED(16) TO QFD-FIELDED-16.
           MOVE FD-FIELDKEY(16) TO QFD-FIELDKEY-16.
           MOVE FD-FIELDNO(17) TO QFD-FIELDNO-17.
           MOVE FD-FIELDROW(17) TO QFD-FIELDROW-17.
           MOVE FD-FIELDCOL(17) TO QFD-FIELDCOL-17.
           MOVE FD-FIELDED(17) TO QFD-FIELDED-17.
           MOVE FD-FIELDKEY(17) TO QFD-FIELDKEY-17.
           MOVE FD-FIELDNO(18) TO QFD-FIELDNO-18.
           MOVE FD-FIELDROW(18) TO QFD-FIELDROW-18.
           MOVE FD-FIELDCOL(18) TO QFD-FIELDCOL-18.
           MOVE FD-FIELDED(18) TO QFD-FIELDED-18.
           MOVE FD-FIELDKEY(18) TO QFD-FIELDKEY-18.
           MOVE FD-FIELDNO(19) TO QFD-FIELDNO-19.
           MOVE FD-FIELDROW(19) TO QFD-FIELDROW-19.
           MOVE FD-FIELDCOL(19) TO QFD-FIELDCOL-19.
           MOVE FD-FIELDED(19) TO QFD-FIELDED-19.
           MOVE FD-FIELDKEY(19) TO QFD-FIELDKEY-19.
           MOVE FD-FIELDNO(20) TO QFD-FIELDNO-20.
           MOVE FD-FIELDROW(20) TO QFD-FIELDROW-20.
           MOVE FD-FIELDCOL(20) TO QFD-FIELDCOL-20.
           MOVE FD-FIELDED(20) TO QFD-FIELDED-20.
           MOVE FD-FIELDKEY(20) TO QFD-FIELDKEY-20.
           MOVE FD-FIELDNO(21) TO QFD-FIELDNO-21.
           MOVE FD-FIELDROW(21) TO QFD-FIELDROW-21.
           MOVE FD-FIELDCOL(21) TO QFD-FIELDCOL-21.
           MOVE FD-FIELDED(21) TO QFD-FIELDED-21.
           MOVE FD-FIELDKEY(21) TO QFD-FIELDKEY-21.
           MOVE FD-FIELDNO(22) TO QFD-FIELDNO-22.
           MOVE FD-FIELDROW(22) TO QFD-FIELDROW-22.
           MOVE FD-FIELDCOL(22) TO QFD-FIELDCOL-22.
           MOVE FD-FIELDED(22) TO QFD-FIELDED-22.
           MOVE FD-FIELDKEY(22) TO QFD-FIELDKEY-22.
           MOVE FD-FIELDNO(23) TO QFD-FIELDNO-23.
           MOVE FD-FIELDROW(23) TO QFD-FIELDROW-23.
           MOVE FD-FIELDCOL(23) TO QFD-FIELDCOL-23.
           MOVE FD-FIELDED(23) TO QFD-FIELDED-23.
           MOVE FD-FIELDKEY(23) TO QFD-FIELDKEY-23.
           MOVE FD-FIELDNO(24) TO QFD-FIELDNO-24.
           MOVE FD-FIELDROW(24) TO QFD-FIELDROW-24.
           MOVE FD-FIELDCOL(24) TO QFD-FIELDCOL-24.
           MOVE FD-FIELDED(24) TO QFD-FIELDED-24.
           MOVE FD-FIELDKEY(24) TO QFD-FIELDKEY-24.
           MOVE FD-FIELDNO(25) TO QFD-FIELDNO-25.
           MOVE FD-FIELDROW(25) TO QFD-FIELDROW-25.
           MOVE FD-FIELDCOL(25) TO QFD-FIELDCOL-25.
           MOVE FD-FIELDED(25) TO QFD-FIELDED-25.
           MOVE FD-FIELDKEY(25) TO QFD-FIELDKEY-25.
           MOVE FD-FIELDNO(26) TO QFD-FIELDNO-26.
           MOVE FD-FIELDROW(26) TO QFD-FIELDROW-26.
           MOVE FD-FIELDCOL(26) TO QFD-FIELDCOL-26.
           MOVE FD-FIELDED(26) TO QFD-FIELDED-26.
           MOVE FD-FIELDKEY(26) TO QFD-FIELDKEY-26.
           MOVE FD-FIELDNO(27) TO QFD-FIELDNO-27.
           MOVE FD-FIELDROW(27) TO QFD-FIELDROW-27.
           MOVE FD-FIELDCOL(27) TO QFD-FIELDCOL-27.
           MOVE FD-FIELDED(27) TO QFD-FIELDED-27.
           MOVE FD-FIELDKEY(27) TO QFD-FIELDKEY-27.
           MOVE FD-FIELDNO(28) TO QFD-FIELDNO-28.
           MOVE FD-FIELDROW(28) TO QFD-FIELDROW-28.
           MOVE FD-FIELDCOL(28) TO QFD-FIELDCOL-28.
           MOVE FD-FIELDED(28) TO QFD-FIELDED-28.
           MOVE FD-FIELDKEY(28) TO QFD-FIELDKEY-28.
           MOVE FD-FIELDNO(29) TO QFD-FIELDNO-29.
           MOVE FD-FIELDROW(29) TO QFD-FIELDROW-29.
           MOVE FD-FIELDCOL(29) TO QFD-FIELDCOL-29.
           MOVE FD-FIELDED(29) TO QFD-FIELDED-29.
           MOVE FD-FIELDKEY(29) TO QFD-FIELDKEY-29.
           MOVE FD-FIELDNO(30) TO QFD-FIELDNO-30.
           MOVE FD-FIELDROW(30) TO QFD-FIELDROW-30.
           MOVE FD-FIELDCOL(30) TO QFD-FIELDCOL-30.
           MOVE FD-FIELDED(30) TO QFD-FIELDED-30.
           MOVE FD-FIELDKEY(30) TO QFD-FIELDKEY-30.
           MOVE FD-FIELDNO(31) TO QFD-FIELDNO-31.
           MOVE FD-FIELDROW(31) TO QFD-FIELDROW-31.
           MOVE FD-FIELDCOL(31) TO QFD-FIELDCOL-31.
           MOVE FD-FIELDED(31) TO QFD-FIELDED-31.
           MOVE FD-FIELDKEY(31) TO QFD-FIELDKEY-31.
           MOVE FD-FIELDNO(32) TO QFD-FIELDNO-32.
           MOVE FD-FIELDROW(32) TO QFD-FIELDROW-32.
           MOVE FD-FIELDCOL(32) TO QFD-FIELDCOL-32.
           MOVE FD-FIELDED(32) TO QFD-FIELDED-32.
           MOVE FD-FIELDKEY(32) TO QFD-FIELDKEY-32.
           MOVE FD-FIELDNO(33) TO QFD-FIELDNO-33.
           MOVE FD-FIELDROW(33) TO QFD-FIELDROW-33.
           MOVE FD-FIELDCOL(33) TO QFD-FIELDCOL-33.
           MOVE FD-FIELDED(33) TO QFD-FIELDED-33.
           MOVE FD-FIELDKEY(33) TO QFD-FIELDKEY-33.
           MOVE FD-FIELDNO(34) TO QFD-FIELDNO-34.
           MOVE FD-FIELDROW(34) TO QFD-FIELDROW-34.
           MOVE FD-FIELDCOL(34) TO QFD-FIELDCOL-34.
           MOVE FD-FIELDED(34) TO QFD-FIELDED-34.
           MOVE FD-FIELDKEY(34) TO QFD-FIELDKEY-34.
           MOVE FD-FIELDNO(35) TO QFD-FIELDNO-35.
           MOVE FD-FIELDROW(35) TO QFD-FIELDROW-35.
           MOVE FD-FIELDCOL(35) TO QFD-FIELDCOL-35.
           MOVE FD-FIELDED(35) TO QFD-FIELDED-35.
           MOVE FD-FIELDKEY(35) TO QFD-FIELDKEY-35.
           MOVE FD-FIELDNO(36) TO QFD-FIELDNO-36.
           MOVE FD-FIELDROW(36) TO QFD-FIELDROW-36.
           MOVE FD-FIELDCOL(36) TO QFD-FIELDCOL-36.
           MOVE FD-FIELDED(36) TO QFD-FIELDED-36.
           MOVE FD-FIELDKEY(36) TO QFD-FIELDKEY-36.
           MOVE FD-FIELDNO(37) TO QFD-FIELDNO-37.
           MOVE FD-FIELDROW(37) TO QFD-FIELDROW-37.
           MOVE FD-FIELDCOL(37) TO QFD-FIELDCOL-37.
           MOVE FD-FIELDED(37) TO QFD-FIELDED-37.
           MOVE FD-FIELDKEY(37) TO QFD-FIELDKEY-37.
           MOVE FD-FIELDNO(38) TO QFD-FIELDNO-38.
           MOVE FD-FIELDROW(38) TO QFD-FIELDROW-38.
           MOVE FD-FIELDCOL(38) TO QFD-FIELDCOL-38.
           MOVE FD-FIELDED(38) TO QFD-FIELDED-38.
           MOVE FD-FIELDKEY(38) TO QFD-FIELDKEY-38.
           MOVE FD-FIELDNO(39) TO QFD-FIELDNO-39.
           MOVE FD-FIELDROW(39) TO QFD-FIELDROW-39.
           MOVE FD-FIELDCOL(39) TO QFD-FIELDCOL-39.
           MOVE FD-FIELDED(39) TO QFD-FIELDED-39.
           MOVE FD-FIELDKEY(39) TO QFD-FIELDKEY-39.
           MOVE FD-FIELDNO(40) TO QFD-FIELDNO-40.
           MOVE FD-FIELDROW(40) TO QFD-FIELDROW-40.
           MOVE FD-FIELDCOL(40) TO QFD-FIELDCOL-40.
           MOVE FD-FIELDED(40) TO QFD-FIELDED-40.
           MOVE FD-FIELDKEY(40) TO QFD-FIELDKEY-40.
           MOVE FD-FIELDNO(41) TO QFD-FIELDNO-41.
           MOVE FD-FIELDROW(41) TO QFD-FIELDROW-41.
           MOVE FD-FIELDCOL(41) TO QFD-FIELDCOL-41.
           MOVE FD-FIELDED(41) TO QFD-FIELDED-41.
           MOVE FD-FIELDKEY(41) TO QFD-FIELDKEY-41.
           MOVE FD-FIELDNO(42) TO QFD-FIELDNO-42.
           MOVE FD-FIELDROW(42) TO QFD-FIELDROW-42.
           MOVE FD-FIELDCOL(42) TO QFD-FIELDCOL-42.
           MOVE FD-FIELDED(42) TO QFD-FIELDED-42.
           MOVE FD-FIELDKEY(42) TO QFD-FIELDKEY-42.
           MOVE FD-FIELDNO(43) TO QFD-FIELDNO-43.
           MOVE FD-FIELDROW(43) TO QFD-FIELDROW-43.
           MOVE FD-FIELDCOL(43) TO QFD-FIELDCOL-43.
           MOVE FD-FIELDED(43) TO QFD-FIELDED-43.
           MOVE FD-FIELDKEY(43) TO QFD-FIELDKEY-43.
           MOVE FD-FIELDNO(44) TO QFD-FIELDNO-44.
           MOVE FD-FIELDROW(44) TO QFD-FIELDROW-44.
           MOVE FD-FIELDCOL(44) TO QFD-FIELDCOL-44.
           MOVE FD-FIELDED(44) TO QFD-FIELDED-44.
           MOVE FD-FIELDKEY(44) TO QFD-FIELDKEY-44.
           MOVE FD-FIELDNO(45) TO QFD-FIELDNO-45.
           MOVE FD-FIELDROW(45) TO QFD-FIELDROW-45.
           MOVE FD-FIELDCOL(45) TO QFD-FIELDCOL-45.
           MOVE FD-FIELDED(45) TO QFD-FIELDED-45.
           MOVE FD-FIELDKEY(45) TO QFD-FIELDKEY-45.
           MOVE FD-FIELDNO(46) TO QFD-FIELDNO-46.
           MOVE FD-FIELDROW(46) TO QFD-FIELDROW-46.
           MOVE FD-FIELDCOL(46) TO QFD-FIELDCOL-46.
           MOVE FD-FIELDED(46) TO QFD-FIELDED-46.
           MOVE FD-FIELDKEY(46) TO QFD-FIELDKEY-46.
           MOVE FD-FIELDNO(47) TO QFD-FIELDNO-47.
           MOVE FD-FIELDROW(47) TO QFD-FIELDROW-47.
           MOVE FD-FIELDCOL(47) TO QFD-FIELDCOL-47.
           MOVE FD-FIELDED(47) TO QFD-FIELDED-47.
           MOVE FD-FIELDKEY(47) TO QFD-FIELDKEY-47.
           MOVE FD-FIELDNO(48) TO QFD-FIELDNO-48.
           MOVE FD-FIELDROW(48) TO QFD-FIELDROW-48.
           MOVE FD-FIELDCOL(48) TO QFD-FIELDCOL-48.
           MOVE FD-FIELDED(48) TO QFD-FIELDED-48.
           MOVE FD-FIELDKEY(48) TO QFD-FIELDKEY-48.
           MOVE FD-FIELDNO(49) TO QFD-FIELDNO-49.
           MOVE FD-FIELDROW(49) TO QFD-FIELDROW-49.
           MOVE FD-FIELDCOL(49) TO QFD-FIELDCOL-49.
           MOVE FD-FIELDED(49) TO QFD-FIELDED-49.
           MOVE FD-FIELDKEY(49) TO QFD-FIELDKEY-49.
           MOVE FD-FIELDNO(50) TO QFD-FIELDNO-50.
           MOVE FD-FIELDROW(50) TO QFD-FIELDROW-50.
           MOVE FD-FIELDCOL(50) TO QFD-FIELDCOL-50.
           MOVE FD-FIELDED(50) TO QFD-FIELDED-50.
           MOVE FD-FIELDKEY(50) TO QFD-FIELDKEY-50.
           MOVE FD-1STFILE TO QFD-1STFILE.
           MOVE FD-INCR-DELCNT TO QFD-INCR-DELCNT.
           MOVE FD-FMTYPE TO QFD-FMTYPE.
           MOVE FD-PRCONT TO QFD-PRCONT.
           MOVE FD-NEXTFM TO QFD-NEXTFM.
           MOVE FD-VALID-BRANCH TO QFD-VALID-BRANCH.
           MOVE FD-MIN-FORM-SENT-DAYS TO QFD-MIN-FORM-SENT-DAYS.
           MOVE FD-SKIP-BAD-ADR TO QFD-SKIP-BAD-ADR.
           MOVE FD-SECURITY-PROFILE TO QFD-SECURITY-PROFILE.
           MOVE FD-FORM-RIGHT-TO-CURE TO QFD-FORM-RIGHT-TO-CURE.
           MOVE FD-TEST-WHICH-STATE TO QFD-TEST-WHICH-STATE.
           MOVE FD-STATE(1) TO QFD-STATE-1.
           MOVE FD-STATE(2) TO QFD-STATE-2.
           MOVE FD-STATE(3) TO QFD-STATE-3.
           MOVE FD-STATE(4) TO QFD-STATE-4.
           MOVE FD-STATE(5) TO QFD-STATE-5.
           MOVE FD-STATE(6) TO QFD-STATE-6.
           MOVE FD-STATE(7) TO QFD-STATE-7.
           MOVE FD-STATE(8) TO QFD-STATE-8.
           MOVE FD-STATE(9) TO QFD-STATE-9.
           MOVE FD-STATE(10) TO QFD-STATE-10.
           MOVE FD-STATE(11) TO QFD-STATE-11.
           MOVE FD-STATE(12) TO QFD-STATE-12.
           MOVE FD-STATE(13) TO QFD-STATE-13.
           MOVE FD-STATE(14) TO QFD-STATE-14.
           MOVE FD-STATE(15) TO QFD-STATE-15.
           MOVE FD-STATE(16) TO QFD-STATE-16.
           MOVE FD-STATE(17) TO QFD-STATE-17.
           MOVE FD-STATE(18) TO QFD-STATE-18.
           MOVE FD-STATE(19) TO QFD-STATE-19.
           MOVE FD-STATE(20) TO QFD-STATE-20.
           MOVE FD-STATE(21) TO QFD-STATE-21.
           MOVE FD-STATE(22) TO QFD-STATE-22.
           MOVE FD-STATE(23) TO QFD-STATE-23.
           MOVE FD-STATE(24) TO QFD-STATE-24.
           MOVE FD-STATE(25) TO QFD-STATE-25.
           MOVE FD-STATE(26) TO QFD-STATE-26.
           MOVE FD-STATE(27) TO QFD-STATE-27.
           MOVE FD-STATE(28) TO QFD-STATE-28.
           MOVE FD-STATE(29) TO QFD-STATE-29.
           MOVE FD-STATE(30) TO QFD-STATE-30.
           MOVE FD-STATE(31) TO QFD-STATE-31.
           MOVE FD-STATE(32) TO QFD-STATE-32.
           MOVE FD-STATE(33) TO QFD-STATE-33.
           MOVE FD-STATE(34) TO QFD-STATE-34.
           MOVE FD-STATE(35) TO QFD-STATE-35.
           MOVE FD-STATE(36) TO QFD-STATE-36.
           MOVE FD-STATE(37) TO QFD-STATE-37.
           MOVE FD-STATE(38) TO QFD-STATE-38.
           MOVE FD-STATE(39) TO QFD-STATE-39.
           MOVE FD-STATE(40) TO QFD-STATE-40.
           MOVE FD-STATE(41) TO QFD-STATE-41.
           MOVE FD-STATE(42) TO QFD-STATE-42.
           MOVE FD-STATE(43) TO QFD-STATE-43.
           MOVE FD-STATE(44) TO QFD-STATE-44.
           MOVE FD-STATE(45) TO QFD-STATE-45.
           MOVE FD-STATE(46) TO QFD-STATE-46.
           MOVE FD-STATE(47) TO QFD-STATE-47.
           MOVE FD-STATE(48) TO QFD-STATE-48.
           MOVE FD-STATE(49) TO QFD-STATE-49.
           MOVE FD-STATE(50) TO QFD-STATE-50.
 
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOFDRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.

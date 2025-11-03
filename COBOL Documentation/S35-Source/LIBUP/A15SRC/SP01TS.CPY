      * COPYMEMBER: LIBSP/SP01TS
      ******************************************************************
      *  EMPLOYEE TIME SHEET FILE
      *  NOTE:  TS-HOURS-WORKED INCLUDES HOURS WORKED PLUS VHS-HOURS1 +
      *         VHS-HOURS2.  DECIDED TO INCLUDE BOTH TIMES AND HOURS 
      *         SINCE IT'S IMPORTANT TO BE ABLE TO SHOW ENTIRE TIME
      *         TICKET AS SUBMITTED.
      *
      *         TS-DATE IS IN YYMMDD FORMAT!!!
      *
      *=================================================================
      * REV:
      * BLF 070425 ADD TS-UPDATE-FG
      * BLF 071108 ADD TS-EM-TYPE
      * BLM 150116 ADDED TS-BATCH, WORLD PR# 1035
      *
      * KEC 2017.0321 {PR#01236} WORLD ACCEPTANCE <A15> [KEVIN MULLEN]
      *         ADDED TS-OT-REASON PIC X(20).
      *
      ******************************************************************
       78  TS-SIZE                       VALUE 230.
       01  TS-REC.
           03  TS2-KEY.
               05  TS-BRNO               PIC 9(4).
               05  TS1-KEY.
                   07  TS-EMPID          PIC 9(6).
                   07  TS-DATE           PIC 9(6).
           03  TS-TIMES                  OCCURS 7.
               05  TS-TIME-IN-AM         PIC 9(4)    COMP-6.
               05  FILLER                REDEFINES TS-TIME-IN-AM.
                   07  TS-TIME-IN-AM-HH  PIC 9(2)    COMP-6.
                   07  TS-TIME-IN-AM-MM  PIC 9(2)    COMP-6.
               05  TS-TIME-OUT-AM        PIC 9(4)    COMP-6.
               05  FILLER                REDEFINES TS-TIME-OUT-AM.
                   07  TS-TIME-OUT-AM-HH PIC 9(2)    COMP-6.
                   07  TS-TIME-OUT-AM-MM PIC 9(2)    COMP-6.
               05  TS-TIME-IN-PM         PIC 9(4)    COMP-6.
               05  FILLER                REDEFINES TS-TIME-IN-PM.
                   07  TS-TIME-IN-PM-HH  PIC 9(2)    COMP-6.
                   07  TS-TIME-IN-PM-MM  PIC 9(2)    COMP-6.
               05  TS-TIME-OUT-PM        PIC 9(4)    COMP-6.
               05  FILLER REDEFINES TS-TIME-OUT-PM.
                   07  TS-TIME-OUT-PM-HH PIC 9(2)    COMP-6.
                   07  TS-TIME-OUT-PM-MM PIC 9(2)    COMP-6.
               05  TS-HOURS-WORKED       PIC 9(2)V99 COMP-6.
               05  TS-VHS-TYPE1          PIC X.
               05  TS-VHS-HOURS1         PIC 9(2)V99 COMP-6.
               05  TS-VHS-TYPE2          PIC X.
               05  TS-VHS-HOURS2         PIC 9(2)V99 COMP-6.
           03  TS-BRANCH-ALLOCATION      OCCURS 6.
               05  TS-BR-WORKED          PIC 9(4)    COMP-6.
               05  TS-BR-HOURS           PIC 9(2)V99 COMP-6.
           03  TS-POST-USERID            PIC X(8).
           03  TS-POST-DATE              PIC 9(6)    COMP-6.
           03  TS-POST-TIME              PIC 9(6)    COMP-6.
           03  TS-APPROVED-BY            PIC X(8).
           03  TS-APPROVED-DATE          PIC 9(6)    COMP-6.
           03  TS-APPROVED-TIME          PIC 9(6)    COMP-6.
           03  TS-UPDATE-FG              PIC X.
           03  TS-EM-TYPE                PIC X.
           03  TS-BATCH                  PIC X(8).
           03  TS-OT-REASON              PIC X(20).
           03  FILLER                    PIC X(20).

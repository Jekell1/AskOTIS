      * COPYMEMBER: LIBSP/SP01VGQ
      ******************************************************************
      *               (VGQ)
      *    VISION VERSION OF THE DAILY COLLECTOR QUEUE FILE
      *
      * VGQ1-KEY = VGQ-COLLID VGQ-SEQ-FG VGQ-DATE VGQ-START-TIME
      *            VGQ-REV-DEL VGQ-MISC-KEY-FLD
      *            VGQ-REV-PDUE-DAYS VGQ-REV-BAL  VGQ-SEQ
      *
      *=================================================================
      * THE PURPOSE OF THE VGQFILE IS FOR SCAN PAGE FUNCTIONALITY
      * THAT CANNOT BE ACHIEVED FOR THE GQ2-KEY IN SQL BUT CAN BE
      * DONE IN VISION.  A PROGRAM WILL LOAD ROWS/RECORDS INTO VGQFILE
      * FOR A GIVEN BRANCH AT THE START AND USE THE FILE FOR ITS NEED.
      *
      * IT IS ALSO NEEDED FOR PROGRAMS THAT REQUIRE A START-GREATER
      * FUNCTION. DUE TO THE COMPLEXITY OF THE GQ2-KEY, GETTING A
      * PROPER WHERE CONSTRAINT TO WORK IN A START-GREATER USAGE IS
      * VERY DIFFICULT.  IT IS EASIER TO USE THE VGQFILE.
      *
      *=================================================================
      * REV:
      * SMR 2024-0205 ***NEW***
      *
      ******************************************************************
       78  VGQ-SIZE                                  VALUE 158.
       78  VGQ-GQ-SIZE                               VALUE 120.
       01  VGQ-REC.
           03  VGQ1-KEY.
               05  VGQ-GQ2-KEY.
                   07  VGQ-COLLID                    PIC XXX.
                   07  VGQ-SEQ-FG                    PIC X.
                   07  VGQ-DATE                      PIC 9(8).
                   07  VGQ-START-TIME                PIC S9(4)   COMP-3.
                   07  VGQ-SEQ-FIELDS.
                       09  VGQ-REV-DEL               PIC 9(5).
                       09  VGQ-MISC-KEY-FLD          PIC X(3).
                       09  VGQ-REV-PDUE-DAYS         PIC 9(5).
                       09  VGQ-REV-BAL               PIC 9(7).
                       09  VGQ-SEQ                   PIC 9(3).
           03  VGQ-GQ-REC                            PIC X(VGQ-GQ-SIZE).



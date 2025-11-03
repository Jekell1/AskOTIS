       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER           PIC 9(3) VALUE 0.
       01  WS-MESSAGE           PIC X(50).
       
       LINKAGE SECTION.
       01  L-INPUT-PARM         PIC X(10).
       01  L-OUTPUT-PARM        PIC 9(5).
       01  L-INOUT-PARM         PIC X(20).
       
       PROCEDURE DIVISION USING L-INPUT-PARM L-OUTPUT-PARM L-INOUT-PARM.
       MAIN-PARA.
           MOVE L-INPUT-PARM TO WS-MESSAGE.
           COMPUTE L-OUTPUT-PARM = WS-COUNTER + 1.
           MOVE "HELLO" TO L-INOUT-PARM.
           MOVE L-INOUT-PARM TO WS-MESSAGE.
           
       END-PARA.
           STOP RUN.

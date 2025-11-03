      * COPYMEMBER: LIBWI/CHREPO.CMD
       01  LEGEND-WALK                   PIC X(50)  VALUE
           "F1-CANCEL, ^-UP, v-DOWN, ENTER FIELD INFORMATION:".
       01  LEGEND-01.
           03  FILLER       PIC X(47)  VALUE
           "F1-RESET, 'DELETE', F2-MEMO, F7-UPDATE AND EXIT".
           03  FILLER       PIC X(24)  VALUE
           " P2, P3, P4 OR FIELD#:  ".
       01  LEGEND-01B.
           03  FILLER       PIC X(49)  VALUE
           "F1-RESET, 'DELETE', F2-MEMO, F7-UPDATE AND EXIT, ".
           03  FILLER       PIC X(20)  VALUE
           "P2, OR ENTER FIELD#:".
       01  LEGEND-02.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F2-MEMO, F7-UPDATE AND EXIT".
           03  FILLER       PIC X(24)  VALUE
           " P1, P3, P4 OR FIELD#:  ".
       01  LEGEND-02B.
           03  FILLER       PIC X(39)  VALUE
           "F1-RESET, F2-MEMO, F7-UPDATE AND EXIT, ".
           03  FILLER       PIC X(19)  VALUE
           "P3 OR ENTER FIELD#:".
       01  LEGEND-03.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F2-MEMO, F7-UPDATE AND EXIT".
           03  FILLER       PIC X(24)  VALUE
           " P1, P2, P4 OR FIELD#:  ".
       01  LEGEND-03B.
           03  FILLER       PIC X(39)  VALUE
           "F1-RESET, F2-MEMO, F7-UPDATE AND EXIT, ".
           03  FILLER       PIC X(19)  VALUE
           "P4 OR ENTER FIELD#:".
       01  LEGEND-04.
           03  FILLER       PIC X(37)  VALUE
           "F1-RESET, F2-MEMO, F7-UPDATE AND EXIT".
           03  FILLER       PIC X(24)  VALUE
           " P1, P2, P3 OR FIELD#:  ".
       01  LEGEND-NEW.
           03  FILLER       PIC X(37)  VALUE
           " F1-CLEAR PAGE,  F7-UPDATE AND EXIT :".

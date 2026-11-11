*> A starter robot for rules v2, on the Clunkyard COBOL binding (clunkyard.cpy
*> and its layer, which the arena provides). It sweeps a medium scan to find
*> a robot, or points it where a passive contact was heard, narrows the scan
*> to hold it, leads it with shots whose power suits the range, fires only
*> when the turret is on target, keeps battery back for driving and keeps
*> out of the storm.
*>
*> The trade-offs (CY-GAME and the CY-GUN- calls have the numbers):
*>   scan     a narrow arc reaches further; a wide one finds robots nearby
*>   power    heavy shots hit harder for less battery, but fly slowly
*>   battery  shots and driving share it, and it refills slowly
*>   turret   it turns at a limited rate; CY-GUN-AIM-NOW is where it points now
*>   cues     passive contacts (a ping, a scan beam, a shot heard) say which
*>            way to look, not where to shoot: they have no id and a noisy
*>            bearing, so CY-SENSORS-NEAREST leaves them out
*> Clunkyard robot API 2.0.0: one core under every language, the same
*> answers everywhere. Facts (a call shows its arguments):
*> match: mode ruleset control robots is_3d tick_rate max_ticks
*>   robot_id(slot)
*> clock: tick seconds remaining_ticks
*> arena: size gravity terrain_kind ground_height(x y)
*> me: health position velocity heading pitch roll angular_velocity
*>   attitude grounded altitude applied
*> battery: charge capacity regen sustainable_drive compute
*>   drive_cost(fraction)
*> drive: hover_lift
*> gun: aim_now cooldown_ticks spread max_power damage_scale
*>   can_fire(power reserve) shot_speed(power) shot_cost(power)
*>   reload_ticks(power) shot_damage(power) power_for_speed(speed)
*>   power_for_flight_time(range seconds) lead(contact power)
*>   lead_point(position velocity power)
*> scanner: last proximity_range reach(arc) arc_for_reach(range)
*> sensors: contacts nearest cues contact(slot) position_of(contact)
*> tracker: cue nearest_bearing_rate velocity(slot) position(slot)
*>   last_seen(slot)
*> nav: zone zone.margin(point) zone.contains(point margin)
*>   zone.distance_from(point) storm storm.margin(point)
*>   storm.contains(point margin) storm.distance_from(point) hill
*>   hill.margin(point) hill.contains(point margin)
*>   hill.distance_from(point) pickups nearest_pickup(kind)
*>   field_distance(a b)
*> score: me best armour
*> angles: normalize(degrees) angle_diff(target current)
*>   bearing_to(from to) elevation_to(from to) direction(heading pitch)
*>   distance(a b) clamp(value low high)
*> raw: number(path) text(path) has(path) flag(path) count(path) line()
*> Controls: drive.set(throttle turn pitch_turn)
*>   drive.forces(thrust strafe lift yaw_torque pitch_torque roll_torque brake)
*>   gun.aim(heading pitch) gun.fire(power) scanner.set(heading arc pitch)
*>   tracker.configure(smoothing forget_ticks cue_ticks)
*>   status.set(text value) raw.extra(json)
*> Every control argument is optional; a second call keeps what the first
*> set. Absent facts are your language's none. Names are snake_case here;
*> your language cases them its own way. Angles are degrees from the +x
*> axis; pitch is positive up.
*> Manual: https://clunkyard.com/manual#sdk
IDENTIFICATION DIVISION.
PROGRAM-ID. BOT.

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY "clunkyard.cpy".
01 FREE-FLIGHT    PIC S9(9) COMP-5 VALUE 0.
01 FLAG           PIC S9(9) COMP-5.
01 SWEEP          PIC S9(9) COMP-5.
*> enough to turn against the brake in the force rulesets
01 SEARCH-TURN    USAGE COMP-2 VALUE 0.33.
01 BRAKE          USAGE COMP-2 VALUE 0.2.
01 ONE-SECOND     USAGE COMP-2 VALUE 1.
01 SHOT-RESERVE   USAGE COMP-2 VALUE 10.
01 UNIT-LOW       USAGE COMP-2 VALUE -1.
01 UNIT-HIGH      USAGE COMP-2 VALUE 1.
01 THROTTLE       USAGE COMP-2.
01 TURN-CONTROL   USAGE COMP-2.
01 PITCH-TURN     USAGE COMP-2.
01 SCAN-HEADING   USAGE COMP-2.
01 SCAN-ARC       USAGE COMP-2.
01 SCAN-PITCH     USAGE COMP-2.
01 SHOT-POWER     USAGE COMP-2.
01 AIM-HEADING    USAGE COMP-2.
01 AIM-PITCH      USAGE COMP-2.
01 BEARING        USAGE COMP-2.
01 ELEVATION      USAGE COMP-2.
01 DISTANCE       USAGE COMP-2.
01 ANGLE-ERROR    USAGE COMP-2.
01 CUE-ERROR      USAGE COMP-2.
01 MARGIN         USAGE COMP-2.
01 WORK           USAGE COMP-2.
*> STEER's input and output: a control towards TOWARDS from CURRENT, full
*> at GAIN degrees off.
01 TOWARDS        USAGE COMP-2.
01 CURRENT        USAGE COMP-2.
01 GAIN           USAGE COMP-2.
01 STEERED        USAGE COMP-2.
*> The calibration kernel's numbers. Each -BITS item is its double's IEEE
*> bits, where one more is the next double up.
01 CAL-WORK       USAGE COMP-2.
01 CAL-ROUNDS     PIC S9(9) COMP-5.
01 CAL-X          USAGE BINARY-DOUBLE UNSIGNED.
01 CAL-SHIFTED    USAGE BINARY-DOUBLE UNSIGNED.
01 CAL-SUM        USAGE COMP-2.
01 CAL-SUM-BITS   REDEFINES CAL-SUM USAGE BINARY-DOUBLE UNSIGNED.
01 CAL-ROOT       USAGE COMP-2.
01 CAL-ROOT-BITS  REDEFINES CAL-ROOT USAGE BINARY-DOUBLE UNSIGNED.
01 CAL-NEAR       USAGE COMP-2.
01 CAL-NEAR-BITS  REDEFINES CAL-NEAR USAGE BINARY-DOUBLE UNSIGNED.
01 CAL-NEXT       USAGE COMP-2.
01 CAL-NEXT-BITS  REDEFINES CAL-NEXT USAGE BINARY-DOUBLE UNSIGNED.
01 CAL-OUT        USAGE COMP-2.

PROCEDURE DIVISION.
MAIN-LOOP.
    PERFORM UNTIL CY-SESSION-EVENT-END
        CALL "CY-READ" USING CY-GAME CY-STATE
        IF RETURN-CODE < 0
            STOP RUN
        END-IF
        EVALUATE TRUE
            WHEN CY-SESSION-EVENT-HELLO PERFORM START-MATCH CALL "CY-SEND"
            WHEN CY-SESSION-EVENT-STATE PERFORM TAKE-TURN CALL "CY-SEND"
        END-EVALUATE
    END-PERFORM
    MOVE 0 TO RETURN-CODE
    STOP RUN.

START-MATCH.
    *> Flying free, robots may be above or below.
    IF CY-MATCH-IS-3D = 1 AND CY-ARENA-GRAVITY-Z = 0
        MOVE 1 TO FREE-FLIGHT
    END-IF.

TAKE-TURN.
    CALL "CY-DRIVE-BRAKE" USING BRAKE
    EVALUATE TRUE
        WHEN CY-SENSORS-NEAREST-FOUND PERFORM TRACK-TARGET
        *> the freshest passive contact, for a few seconds
        WHEN CY-TRACKER-CUE-FOUND PERFORM FOLLOW-CUE
        WHEN OTHER PERFORM SEARCH-AROUND
    END-EVALUATE
    *> Low battery: drive gently so the shots can still be paid for.
    IF CY-BATTERY-CHARGE < CY-BATTERY-CAPACITY / 4
        CALL "CY-DRIVE-GET-THROTTLE" USING THROTTLE
        COMPUTE THROTTLE = THROTTLE * 0.5
        CALL "CY-DRIVE-THROTTLE" USING THROTTLE
    END-IF
    *> The storm hurts inside: near its edge, drive straight away from its centre.
    IF CY-NAV-STORM-FOUND
        CALL "CY-NAV-STORM-MARGIN" USING CY-ME-POSITION MARGIN
        IF RETURN-CODE = CY-OK AND MARGIN > -60
            CALL "CY-ANGLES-BEARING-TO" USING CY-NAV-STORM-CENTER CY-ME-POSITION TOWARDS
            MOVE CY-ME-HEADING TO CURRENT
            MOVE 30 TO GAIN
            PERFORM STEER
            MOVE 0.8 TO THROTTLE
            CALL "CY-DRIVE-THROTTLE" USING THROTTLE
            CALL "CY-DRIVE-TURN" USING STEERED
        END-IF
    END-IF
    *> Sudden death: head back inside the safe zone.
    IF CY-NAV-ZONE-FOUND
        CALL "CY-NAV-ZONE-MARGIN" USING CY-ME-POSITION MARGIN
        IF RETURN-CODE = CY-OK AND MARGIN < 60
            MOVE 0 TO ELEVATION
            IF CY-NAV-ZONE-SHAPE-SPHERE
                CALL "CY-ANGLES-ELEVATION-TO" USING CY-ME-POSITION CY-NAV-ZONE-CENTER ELEVATION
            END-IF
            MOVE 30 TO GAIN
            CALL "CY-ANGLES-BEARING-TO" USING CY-ME-POSITION CY-NAV-ZONE-CENTER TOWARDS
            MOVE CY-ME-HEADING TO CURRENT
            PERFORM STEER
            MOVE STEERED TO TURN-CONTROL
            MOVE ELEVATION TO TOWARDS
            MOVE CY-ME-PITCH TO CURRENT
            PERFORM STEER
            MOVE 0.8 TO THROTTLE
            CALL "CY-DRIVE-THROTTLE" USING THROTTLE
            CALL "CY-DRIVE-TURN" USING TURN-CONTROL
            CALL "CY-DRIVE-PITCH-TURN" USING STEERED
        END-IF
    END-IF
    PERFORM CALIBRATE.

TRACK-TARGET.
    EVALUATE TRUE
        WHEN CY-SENSORS-NEAREST-RANGE > 350 MOVE 0.7 TO THROTTLE
        WHEN CY-SENSORS-NEAREST-RANGE < 200 MOVE -0.4 TO THROTTLE
        WHEN OTHER MOVE 0.2 TO THROTTLE
    END-EVALUATE
    MOVE 45 TO GAIN
    MOVE CY-SENSORS-NEAREST-BEARING TO TOWARDS
    MOVE CY-ME-HEADING TO CURRENT
    PERFORM STEER
    MOVE STEERED TO TURN-CONTROL
    MOVE CY-SENSORS-NEAREST-ELEVATION TO TOWARDS
    MOVE CY-ME-PITCH TO CURRENT
    PERFORM STEER
    CALL "CY-DRIVE-THROTTLE" USING THROTTLE
    CALL "CY-DRIVE-TURN" USING TURN-CONTROL
    CALL "CY-DRIVE-PITCH-TURN" USING STEERED
    *> Narrow: holds it, reaching about 735.
    MOVE 20 TO SCAN-ARC
    CALL "CY-SCANNER-HEADING" USING CY-SENSORS-NEAREST-BEARING
    CALL "CY-SCANNER-ARC" USING SCAN-ARC
    CALL "CY-SCANNER-PITCH" USING CY-SENSORS-NEAREST-ELEVATION
    *> The heaviest shot that still covers the range in about a second,
    *> aimed where the target will be, lobbed under gravity.
    CALL "CY-GUN-POWER-FOR-FLIGHT-TIME" USING CY-SENSORS-NEAREST-RANGE ONE-SECOND SHOT-POWER
    CALL "CY-GUN-LEAD" USING CY-SENSORS-NEAREST SHOT-POWER CY-SOLUTION
    IF RETURN-CODE = CY-OK
        MOVE CY-SOLUTION-HEADING TO AIM-HEADING
        MOVE CY-SOLUTION-PITCH TO AIM-PITCH
    ELSE
        MOVE CY-SENSORS-NEAREST-BEARING TO AIM-HEADING
        MOVE CY-SENSORS-NEAREST-ELEVATION TO AIM-PITCH
    END-IF
    CALL "CY-GUN-AIM-HEADING" USING AIM-HEADING
    CALL "CY-GUN-AIM-PITCH" USING AIM-PITCH
    CALL "CY-ANGLES-ANGLE-DIFF" USING AIM-HEADING CY-GUN-AIM-NOW-HEADING ANGLE-ERROR
    CALL "CY-GUN-CAN-FIRE" USING SHOT-POWER SHOT-RESERVE FLAG
    IF FUNCTION ABS(ANGLE-ERROR) < 3 AND FUNCTION ABS(AIM-PITCH - CY-GUN-AIM-NOW-PITCH) < 3
            AND FLAG = 1
        CALL "CY-GUN-FIRE"
        CALL "CY-GUN-POWER" USING SHOT-POWER
    END-IF
    CALL "CY-STATUS-TEXT" USING Z"TRACK"
    CALL "CY-STATUS-VALUE" USING CY-SENSORS-NEAREST-RANGE.

FOLLOW-CUE.
    *> Head for where it was heard, sweeping an arc that still reaches it
    *> across the bearing's error (and, flying free, the elevation's).
    CALL "CY-ANGLES-BEARING-TO" USING CY-ME-POSITION CY-TRACKER-CUE-POSITION BEARING
    CALL "CY-ANGLES-DISTANCE" USING CY-ME-POSITION CY-TRACKER-CUE-POSITION DISTANCE
    MOVE 0 TO ELEVATION
    IF FREE-FLIGHT = 1
        CALL "CY-ANGLES-ELEVATION-TO" USING CY-ME-POSITION CY-TRACKER-CUE-POSITION ELEVATION
    END-IF
    MOVE CY-TRACKER-CUE-BEARING-ERROR TO CUE-ERROR
    IF CUE-ERROR > 30
        MOVE 30 TO CUE-ERROR
    END-IF
    COMPUTE SWEEP = FUNCTION MOD(CY-CLOCK-TICK, 5) - 2
    COMPUTE SCAN-HEADING = BEARING + SWEEP * CUE-ERROR / 2
    MOVE ELEVATION TO SCAN-PITCH
    IF FREE-FLIGHT = 1
        DIVIDE CY-CLOCK-TICK BY 5 GIVING SWEEP
        COMPUTE SWEEP = FUNCTION MOD(SWEEP, 3) - 1
        COMPUTE SCAN-PITCH = ELEVATION + SWEEP * CUE-ERROR / 2
    END-IF
    COMPUTE WORK = DISTANCE + 100
    CALL "CY-SCANNER-ARC-FOR-REACH" USING WORK SCAN-ARC
    CALL "CY-SCANNER-HEADING" USING SCAN-HEADING
    CALL "CY-SCANNER-ARC" USING SCAN-ARC
    CALL "CY-SCANNER-PITCH" USING SCAN-PITCH
    MOVE 45 TO GAIN
    MOVE BEARING TO TOWARDS
    MOVE CY-ME-HEADING TO CURRENT
    PERFORM STEER
    MOVE STEERED TO TURN-CONTROL
    MOVE ELEVATION TO TOWARDS
    MOVE CY-ME-PITCH TO CURRENT
    PERFORM STEER
    MOVE 0.7 TO THROTTLE
    CALL "CY-DRIVE-THROTTLE" USING THROTTLE
    CALL "CY-DRIVE-TURN" USING TURN-CONTROL
    CALL "CY-DRIVE-PITCH-TURN" USING STEERED
    CALL "CY-STATUS-TEXT" USING Z"CUE"
    CALL "CY-STATUS-VALUE" USING DISTANCE.

SEARCH-AROUND.
    MOVE 45 TO GAIN
    MOVE 0 TO TOWARDS
    MOVE CY-ME-PITCH TO CURRENT
    PERFORM STEER
    MOVE 0.5 TO THROTTLE
    CALL "CY-DRIVE-THROTTLE" USING THROTTLE
    CALL "CY-DRIVE-TURN" USING SEARCH-TURN
    CALL "CY-DRIVE-PITCH-TURN" USING STEERED
    *> Sweeps the circle in half a second, flying free at three heights in turn.
    COMPUTE WORK = CY-SCANNER-LAST-HEADING + 36
    CALL "CY-ANGLES-NORMALIZE" USING WORK SCAN-HEADING
    MOVE 40 TO SCAN-ARC
    MOVE 0 TO SCAN-PITCH
    IF FREE-FLIGHT = 1
        DIVIDE CY-CLOCK-TICK BY 10 GIVING SWEEP
        COMPUTE SWEEP = FUNCTION MOD(SWEEP, 3) - 1
        COMPUTE SCAN-PITCH = SWEEP * 35
    END-IF
    CALL "CY-SCANNER-HEADING" USING SCAN-HEADING
    CALL "CY-SCANNER-ARC" USING SCAN-ARC
    CALL "CY-SCANNER-PITCH" USING SCAN-PITCH
    CALL "CY-STATUS-TEXT" USING Z"SEARCH".

*> A turn or pitch-turn control towards TOWARDS from CURRENT, full at GAIN
*> degrees off, into STEERED.
STEER.
    CALL "CY-ANGLES-ANGLE-DIFF" USING TOWARDS CURRENT ANGLE-ERROR
    COMPUTE WORK = ANGLE-ERROR / GAIN
    CALL "CY-ANGLES-CLAMP" USING WORK UNIT-LOW UNIT-HIGH STEERED.

*> ---- The arena's rate calibration: safe to delete, with its PERFORM ----
*> Only the calibration sends calibration.work; it times this language on
*> the same work as every other. The kernel is xorshift-sqrt v1: work rounds
*> of a 64-bit xorshift and a square-root sum, reported as WORK. COBOL has
*> no bitwise operators, so CBL_XOR does XOR and shifts multiply or divide
*> by powers of two. GnuCOBOL works COMP-2 arithmetic in decimal and
*> truncates the result, so each root and each sum is rounded to the
*> nearest double by hand.
CALIBRATE.
    MOVE 0 TO CAL-WORK
    CALL "CY-RAW-NUMBER" USING Z"state.calibration.work" CAL-WORK
    IF CAL-WORK < 1
        EXIT PARAGRAPH
    END-IF
    MOVE 88172645463325252 TO CAL-X
    MOVE CY-CLOCK-TICK TO CAL-SHIFTED
    CALL "CBL_XOR" USING CAL-SHIFTED CAL-X BY VALUE 8
    MOVE CAL-WORK TO CAL-ROUNDS
    MOVE 0 TO CAL-SUM
    PERFORM CAL-ROUNDS TIMES
        *> x << 13 keeps the low 51 bits, and x << 17 the low 47.
        COMPUTE CAL-SHIFTED = FUNCTION MOD(CAL-X, 2 ** 51) * 2 ** 13
        CALL "CBL_XOR" USING CAL-SHIFTED CAL-X BY VALUE 8
        COMPUTE CAL-SHIFTED = CAL-X / 2 ** 7
        CALL "CBL_XOR" USING CAL-SHIFTED CAL-X BY VALUE 8
        COMPUTE CAL-SHIFTED = FUNCTION MOD(CAL-X, 2 ** 47) * 2 ** 17
        CALL "CBL_XOR" USING CAL-SHIFTED CAL-X BY VALUE 8
        COMPUTE CAL-SHIFTED = CAL-X / 2 ** 11
        PERFORM CALIBRATION-ROOT
        PERFORM CALIBRATION-ADD
    END-PERFORM
    COMPUTE CAL-SHIFTED = CAL-X / 2 ** 40
    COMPUTE CAL-OUT = FUNCTION MOD(FUNCTION INTEGER(CAL-SUM) + CAL-SHIFTED, 1000000)
    CALL "CY-STATUS-TEXT" USING Z"WORK"
    CALL "CY-STATUS-VALUE" USING CAL-OUT.

*> The square root of CAL-SHIFTED to the nearest double: FUNCTION SQRT
*> comes close, and a neighbour is nearer while the midpoint between
*> them is on the root's side.
CALIBRATION-ROOT.
    COMPUTE CAL-ROOT = FUNCTION SQRT(CAL-SHIFTED)
    COMPUTE CAL-NEXT-BITS = CAL-ROOT-BITS + 1
    PERFORM UNTIL (CAL-ROOT + CAL-NEXT) * (CAL-ROOT + CAL-NEXT) > 4 * CAL-SHIFTED
        MOVE CAL-NEXT-BITS TO CAL-ROOT-BITS
        ADD 1 TO CAL-NEXT-BITS
    END-PERFORM
    COMPUTE CAL-NEXT-BITS = CAL-ROOT-BITS - 1
    PERFORM UNTIL CAL-ROOT-BITS = 0
            OR (CAL-ROOT + CAL-NEXT) * (CAL-ROOT + CAL-NEXT) < 4 * CAL-SHIFTED
        MOVE CAL-NEXT-BITS TO CAL-ROOT-BITS
        SUBTRACT 1 FROM CAL-NEXT-BITS
    END-PERFORM.

*> CAL-SUM + CAL-ROOT to the nearest double, ties to even: the truncated
*> sum, or the next double up when the exact sum is nearer to that.
CALIBRATION-ADD.
    COMPUTE CAL-NEAR = CAL-SUM + CAL-ROOT
    COMPUTE CAL-NEXT-BITS = CAL-NEAR-BITS + 1
    IF 2 * (CAL-SUM + CAL-ROOT) > CAL-NEAR + CAL-NEXT
            OR (2 * (CAL-SUM + CAL-ROOT) = CAL-NEAR + CAL-NEXT AND FUNCTION MOD(CAL-NEXT-BITS, 2) = 0)
        MOVE CAL-NEXT-BITS TO CAL-SUM-BITS
    ELSE
        MOVE CAL-NEAR-BITS TO CAL-SUM-BITS
    END-IF.

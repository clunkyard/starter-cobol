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
*>   attitude grounded altitude wall_contact applied hull_mounts
*>   to_mount(heading pitch) from_mount(heading pitch) mount_toward(point)
*> battery: charge capacity regen sustainable_drive compute
*>   drive_cost(fraction)
*> drive: hover_lift
*> gun: aim_now cooldown_ticks spread max_power damage_scale
*>   can_fire(power reserve) shot_speed(power) shot_cost(power)
*>   reload_ticks(power) shot_damage(power) power_for_speed(speed)
*>   power_for_flight_time(range seconds) lead(contact power)
*>   lead_point(position velocity power)
*> scanner: last proximity_range reach(arc) arc_for_reach(range) is_on
*> sensors: contacts nearest cues contact(slot) position_of(contact)
*> tracker: cue nearest_bearing_rate velocity(slot) position(slot)
*>   history(slot) last_seen(slot)
*> nav: zone zone.margin(point) zone.contains(point margin)
*>   zone.distance_from(point) storm storm.margin(point)
*>   storm.contains(point margin) storm.distance_from(point) hill
*>   hill.margin(point) hill.contains(point margin)
*>   hill.distance_from(point) wall_margin() pickups nearest_pickup(kind)
*>   field_distance(a b)
*> score: me best armour
*> angles: normalize(degrees) angle_diff(target current)
*>   bearing_to(from to) elevation_to(from to) direction(heading pitch)
*>   distance(a b) to_chassis(heading pitch attitude)
*>   from_chassis(heading pitch attitude) clamp(value low high)
*> raw: number(path) text(path) has(path) flag(path) count(path) line()
*> Controls: drive.set(throttle turn pitch_turn)
*>   drive.forces(thrust strafe lift yaw_torque pitch_torque roll_torque brake)
*>   gun.aim(heading pitch) gun.fire(power) scanner.set(heading arc pitch)
*>   scanner.power(on) tracker.configure(smoothing forget_ticks cue_ticks)
*>   status.set(text value) debug.log(line) raw.extra(json)
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
*> The dodge: ticks left, dodges taken, and its controls this tick.
01 DODGE-TICKS    PIC S9(9) COMP-5 VALUE 0.
01 DODGES         PIC S9(9) COMP-5 VALUE 0.
01 DODGE-THROTTLE USAGE COMP-2.
01 DODGE-SIDE     USAGE COMP-2.
01 DODGE-COUNT    USAGE COMP-2.
01 CUE-INDEX      PIC S9(9) COMP-5.
01 FIRED          PIC S9(9) COMP-5.
*> Bearing is earned: a scan contact's bearing is off by up to its
*> bearing error (degrees, a fixed share of the arc), so a wide beam finds a
*> robot and a narrow one aims at it. Contacts within 120 units are exact.
01 SEARCH-ARC     USAGE COMP-2 VALUE 40.
*> units across at the target: it stays in the beam from tick to tick
01 HOLD-WIDTH     USAGE COMP-2 VALUE 60.
*> degrees: the narrowest the beam holds
01 MIN-ARC        USAGE COMP-2 VALUE 6.
*> ticks of sightings the aim averages
01 WINDOW-TICKS   PIC S9(9) COMP-5 VALUE 12.
*> narrow sightings in the window before a shot
01 SIGHTINGS-NEEDED PIC S9(9) COMP-5 VALUE 8.
*> ticks between looks around the target, for a nearer robot
01 LOOK-EVERY     PIC S9(9) COMP-5 VALUE 40.
*> ticks to look again where a lost target was
01 REACQUIRE-TICKS PIC S9(9) COMP-5 VALUE 10.
01 HOLD-ARC       USAGE COMP-2.
01 READY          PIC S9(9) COMP-5.
01 LAST-SLOT      PIC S9(9) COMP-5 VALUE 0.
01 LOST           PIC S9(9) COMP-5.
01 SEEN-TICK      PIC S9(9) COMP-5.
01 SPOT.
   05 SPOT-X      USAGE COMP-2.
   05 SPOT-Y      USAGE COMP-2.
   05 SPOT-Z      USAGE COMP-2.
*> The fit: a line through recent sightings (see FIT-LINE).
01 HISTORY-TABLE.
   05 SIGHTING OCCURS 32.
      10 SIGHT-TICK          PIC S9(9) COMP-5.
      10 SIGHT-SOURCE        PIC S9(9) COMP-5.
      10 SIGHT-BEARING-ERROR USAGE COMP-2.
      10 SIGHT-RANGE-ERROR   USAGE COMP-2.
      10 SIGHT-X             USAGE COMP-2.
      10 SIGHT-Y             USAGE COMP-2.
      10 SIGHT-Z             USAGE COMP-2.
01 HISTORY-CAPACITY PIC S9(9) COMP-5 VALUE 32.
01 HISTORY-COUNT  PIC S9(9) COMP-5.
01 ROW            PIC S9(9) COMP-5.
01 FIT-COUNT      PIC S9(9) COMP-5.
01 FIT-POSITION.
   05 FIT-POSITION-X USAGE COMP-2.
   05 FIT-POSITION-Y USAGE COMP-2.
   05 FIT-POSITION-Z USAGE COMP-2.
01 FIT-VELOCITY.
   05 FIT-VELOCITY-X USAGE COMP-2.
   05 FIT-VELOCITY-Y USAGE COMP-2.
   05 FIT-VELOCITY-Z USAGE COMP-2.
01 CUTOFF         USAGE COMP-2.
01 BEST-ERROR     USAGE COMP-2.
01 MEAN-TICK      USAGE COMP-2.
01 MEAN-X         USAGE COMP-2.
01 MEAN-Y         USAGE COMP-2.
01 MEAN-Z         USAGE COMP-2.
01 SUM-T          USAGE COMP-2.
01 SUM-X          USAGE COMP-2.
01 SUM-Y          USAGE COMP-2.
01 SUM-Z          USAGE COMP-2.
01 SPREAD         USAGE COMP-2.
01 DT             USAGE COMP-2.
01 AHEAD          USAGE COMP-2.
*> STEER's input and output: a control towards TOWARDS from CURRENT, full
*> at GAIN degrees off.
01 TOWARDS        USAGE COMP-2.
01 CURRENT        USAGE COMP-2.
01 GAIN           USAGE COMP-2.
01 STEERED        USAGE COMP-2.
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
    MOVE 0 TO LOST
    IF NOT CY-SENSORS-NEAREST-FOUND AND LAST-SLOT NOT = 0
        CALL "CY-TRACKER-LAST-SEEN" USING LAST-SLOT SEEN-TICK
        IF RETURN-CODE = CY-OK AND CY-CLOCK-TICK - SEEN-TICK <= REACQUIRE-TICKS
            CALL "CY-TRACKER-POSITION" USING LAST-SLOT SPOT
            IF RETURN-CODE = CY-OK
                MOVE 1 TO LOST
            END-IF
        END-IF
    END-IF
    EVALUATE TRUE
        WHEN CY-SENSORS-NEAREST-FOUND PERFORM TRACK-TARGET
        *> lost it: look again where it was last seen
        WHEN LOST = 1 PERFORM REACQUIRE
        *> the freshest passive contact, for a few seconds
        WHEN CY-TRACKER-CUE-FOUND PERFORM FOLLOW-CUE
        WHEN OTHER PERFORM SEARCH-AROUND
    END-EVALUATE
    PERFORM DODGE
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
    CONTINUE.

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
    MOVE CY-SENSORS-NEAREST-SLOT TO LAST-SLOT
    *> Halve the beam each tick until it is HOLD-WIDTH across at the target:
    *> narrow enough for a precise bearing, wide enough to keep it in.
    COMPUTE HOLD-ARC = 2 * FUNCTION ATAN(HOLD-WIDTH / 2 / CY-SENSORS-NEAREST-RANGE) * 180 / FUNCTION PI
    IF HOLD-ARC < MIN-ARC
        MOVE MIN-ARC TO HOLD-ARC
    END-IF
    *> In a crowd a narrow beam sees one robot: every LOOK-EVERY ticks,
    *> widen it for a tick.
    IF FUNCTION MOD(CY-CLOCK-TICK, LOOK-EVERY) = 0
        MOVE SEARCH-ARC TO SCAN-ARC
    ELSE
        COMPUTE SCAN-ARC = CY-SCANNER-LAST-ARC / 2
        IF SCAN-ARC > SEARCH-ARC
            MOVE SEARCH-ARC TO SCAN-ARC
        END-IF
        IF SCAN-ARC < HOLD-ARC
            MOVE HOLD-ARC TO SCAN-ARC
        END-IF
    END-IF
    CALL "CY-SCANNER-HEADING" USING CY-SENSORS-NEAREST-BEARING
    CALL "CY-SCANNER-ARC" USING SCAN-ARC
    CALL "CY-SCANNER-PITCH" USING CY-SENSORS-NEAREST-ELEVATION
    *> The heaviest shot that still covers the range in about a second.
    CALL "CY-GUN-POWER-FOR-FLIGHT-TIME" USING CY-SENSORS-NEAREST-RANGE ONE-SECOND SHOT-POWER
    *> An exact contact: the lead is the intercept if the target holds
    *> course. A noisy one: lead the fitted line, and fire only once it
    *> rests on enough sightings.
    MOVE 1 TO READY
    IF CY-SENSORS-NEAREST-BEARING-ERROR > 0
        PERFORM FIT-LINE
        MOVE 1 TO RETURN-CODE
        IF FIT-COUNT >= 2
            CALL "CY-GUN-LEAD-POINT" USING FIT-POSITION FIT-VELOCITY SHOT-POWER CY-SOLUTION
        END-IF
        IF RETURN-CODE NOT = CY-OK OR FIT-COUNT < SIGHTINGS-NEEDED
            MOVE 0 TO READY
        END-IF
    ELSE
        CALL "CY-GUN-LEAD" USING CY-SENSORS-NEAREST SHOT-POWER CY-SOLUTION
    END-IF
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
            AND FLAG = 1 AND READY = 1
        CALL "CY-GUN-FIRE"
        CALL "CY-GUN-POWER" USING SHOT-POWER
    END-IF
    IF READY = 1
        CALL "CY-STATUS-TEXT" USING Z"TRACK"
    ELSE
        CALL "CY-STATUS-TEXT" USING Z"NARROW"
    END-IF
    CALL "CY-STATUS-VALUE" USING CY-SENSORS-NEAREST-RANGE.

*> A straight line through the narrowest of the last WINDOW-TICKS ticks'
*> sightings: where the robot is now (FIT-POSITION) and how fast it moves
*> (FIT-VELOCITY). Each sighting is off by its bearing error; the line
*> averages that out. FIT-COUNT is the sightings used, under 2 for no fit.
FIT-LINE.
    MOVE 0 TO FIT-COUNT
    MOVE 0 TO HISTORY-COUNT
    CALL "CY-TRACKER-HISTORY" USING LAST-SLOT HISTORY-TABLE HISTORY-CAPACITY HISTORY-COUNT
    IF RETURN-CODE NOT = CY-OK
        MOVE 0 TO HISTORY-COUNT
    END-IF
    COMPUTE CUTOFF = CY-CLOCK-TICK - WINDOW-TICKS
    MOVE 1000000 TO BEST-ERROR
    PERFORM VARYING ROW FROM 1 BY 1 UNTIL ROW > HISTORY-COUNT
        IF SIGHT-TICK(ROW) > CUTOFF AND SIGHT-BEARING-ERROR(ROW) < BEST-ERROR
            MOVE SIGHT-BEARING-ERROR(ROW) TO BEST-ERROR
        END-IF
    END-PERFORM
    COMPUTE BEST-ERROR = BEST-ERROR * 1.5
    MOVE 0 TO SUM-T SUM-X SUM-Y SUM-Z
    PERFORM VARYING ROW FROM 1 BY 1 UNTIL ROW > HISTORY-COUNT
        IF SIGHT-TICK(ROW) > CUTOFF AND SIGHT-BEARING-ERROR(ROW) <= BEST-ERROR
            ADD 1 TO FIT-COUNT
            ADD SIGHT-TICK(ROW) TO SUM-T
            ADD SIGHT-X(ROW) TO SUM-X
            ADD SIGHT-Y(ROW) TO SUM-Y
            ADD SIGHT-Z(ROW) TO SUM-Z
        END-IF
    END-PERFORM
    IF FIT-COUNT >= 2
        COMPUTE MEAN-TICK = SUM-T / FIT-COUNT
        COMPUTE MEAN-X = SUM-X / FIT-COUNT
        COMPUTE MEAN-Y = SUM-Y / FIT-COUNT
        COMPUTE MEAN-Z = SUM-Z / FIT-COUNT
        MOVE 0 TO SPREAD SUM-X SUM-Y SUM-Z
        PERFORM VARYING ROW FROM 1 BY 1 UNTIL ROW > HISTORY-COUNT
            IF SIGHT-TICK(ROW) > CUTOFF AND SIGHT-BEARING-ERROR(ROW) <= BEST-ERROR
                COMPUTE DT = SIGHT-TICK(ROW) - MEAN-TICK
                COMPUTE SPREAD = SPREAD + DT * DT
                COMPUTE SUM-X = SUM-X + DT * (SIGHT-X(ROW) - MEAN-X)
                COMPUTE SUM-Y = SUM-Y + DT * (SIGHT-Y(ROW) - MEAN-Y)
                COMPUTE SUM-Z = SUM-Z + DT * (SIGHT-Z(ROW) - MEAN-Z)
            END-IF
        END-PERFORM
        COMPUTE AHEAD = CY-CLOCK-TICK - MEAN-TICK
        COMPUTE FIT-POSITION-X = MEAN-X + SUM-X / SPREAD * AHEAD
        COMPUTE FIT-POSITION-Y = MEAN-Y + SUM-Y / SPREAD * AHEAD
        COMPUTE FIT-POSITION-Z = MEAN-Z + SUM-Z / SPREAD * AHEAD
        COMPUTE FIT-VELOCITY-X = SUM-X / SPREAD * CY-MATCH-TICK-RATE
        COMPUTE FIT-VELOCITY-Y = SUM-Y / SPREAD * CY-MATCH-TICK-RATE
        COMPUTE FIT-VELOCITY-Z = SUM-Z / SPREAD * CY-MATCH-TICK-RATE
    END-IF.

REACQUIRE.
    *> Lost it: look again where it was last seen, widening the beam.
    CALL "CY-ANGLES-BEARING-TO" USING CY-ME-POSITION SPOT BEARING
    MOVE 0 TO ELEVATION
    IF FREE-FLIGHT = 1
        CALL "CY-ANGLES-ELEVATION-TO" USING CY-ME-POSITION SPOT ELEVATION
    END-IF
    MOVE 45 TO GAIN
    MOVE BEARING TO TOWARDS
    MOVE CY-ME-HEADING TO CURRENT
    PERFORM STEER
    MOVE STEERED TO TURN-CONTROL
    MOVE ELEVATION TO TOWARDS
    MOVE CY-ME-PITCH TO CURRENT
    PERFORM STEER
    MOVE 0.5 TO THROTTLE
    CALL "CY-DRIVE-THROTTLE" USING THROTTLE
    CALL "CY-DRIVE-TURN" USING TURN-CONTROL
    CALL "CY-DRIVE-PITCH-TURN" USING STEERED
    COMPUTE SCAN-ARC = CY-SCANNER-LAST-ARC * 2
    IF SCAN-ARC > SEARCH-ARC
        MOVE SEARCH-ARC TO SCAN-ARC
    END-IF
    CALL "CY-SCANNER-HEADING" USING BEARING
    CALL "CY-SCANNER-ARC" USING SCAN-ARC
    CALL "CY-SCANNER-PITCH" USING ELEVATION
    CALL "CY-STATUS-TEXT" USING Z"REACQUIRE".

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

*> A simple dodge: vary it, or predict the shooter. A shot heard starts
*> half a second off the line of fire: odd dodges go left, even ones
*> right. Turn +1 and strafe -1 go left.
DODGE.
    MOVE 0 TO FIRED
    IF CY-SENSORS-CUES-FOUND
        PERFORM VARYING CUE-INDEX FROM 1 BY 1 UNTIL CUE-INDEX > CY-SENSORS-CUES-COUNT
            IF CY-SENSORS-CUES-SOURCE-MUZZLE(CUE-INDEX)
                MOVE 1 TO FIRED
            END-IF
        END-PERFORM
    END-IF
    IF DODGE-TICKS = 0 AND FIRED = 1
        ADD 1 TO DODGES
        MOVE 10 TO DODGE-TICKS
    END-IF
    IF DODGE-TICKS > 0
        SUBTRACT 1 FROM DODGE-TICKS
        IF FUNCTION MOD(DODGES, 2) = 1
            MOVE 1 TO DODGE-SIDE
        ELSE
            MOVE -1 TO DODGE-SIDE
        END-IF
        IF CY-MATCH-CONTROL-FORCES
            MOVE 0.5 TO DODGE-THROTTLE
            CALL "CY-DRIVE-THROTTLE" USING DODGE-THROTTLE
            COMPUTE DODGE-SIDE = 0 - DODGE-SIDE
            CALL "CY-DRIVE-STRAFE" USING DODGE-SIDE
        ELSE
            MOVE 1 TO DODGE-THROTTLE
            CALL "CY-DRIVE-THROTTLE" USING DODGE-THROTTLE
            CALL "CY-DRIVE-TURN" USING DODGE-SIDE
        END-IF
        IF FUNCTION MOD(DODGES, 2) = 1
            CALL "CY-STATUS-TEXT" USING Z"dodge:left"
        ELSE
            CALL "CY-STATUS-TEXT" USING Z"dodge:right"
        END-IF
        MOVE DODGES TO DODGE-COUNT
        CALL "CY-STATUS-VALUE" USING DODGE-COUNT
    END-IF.

*> A turn or pitch-turn control towards TOWARDS from CURRENT, full at GAIN
*> degrees off, into STEERED.
STEER.
    CALL "CY-ANGLES-ANGLE-DIFF" USING TOWARDS CURRENT ANGLE-ERROR
    COMPUTE WORK = ANGLE-ERROR / GAIN
    CALL "CY-ANGLES-CLAMP" USING WORK UNIT-LOW UNIT-HIGH STEERED.

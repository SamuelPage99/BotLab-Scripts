droneManagement : BotDecisionContext -> SeeUndockingComplete -> List OverviewWindowEntry -> DecisionPathNode
droneManagement context seeUndockingComplete overviewEntriesToLock =
    if (context.readingFromGameClient |> shipManeuverIsOrbit) || (context.readingFromGameClient |> shipManeuverIsApproach) then
        describeBranch "Managing drones while traveling."
            (if distanceFromLockedTarget context < 19000 && (distanceFromLockedTarget context >= 5000) then
                returnGroupDronesToBay context "hobgoblin"
                    |> Maybe.withDefault
                        (returnGroupDronesToBay context "bouncer"
                            |> Maybe.withDefault
                                (returnGroupDronesToBay context "warden"
                                    |> Maybe.withDefault
                                        (returnGroupDronesToBay context "garde"
                                            |> Maybe.withDefault
                                                (launchAndEngageGroupNameDrones context.readingFromGameClient context "gecko"
                                                    |> Maybe.withDefault
                                                        (describeBranch "No idling drones."
                                                            (if context.eventContext.appSettings.maxTargetCount <= (context.readingFromGameClient.targets |> List.length) then
                                                                describeBranch "Enough locked combat targets." waitForProgressInGame

                                                             else
                                                                case overviewEntriesToLock of
                                                                    [] ->
                                                                        describeBranch "I see no more overview combat entries to lock." waitForProgressInGame

                                                                    nextOverviewEntryToLock :: _ ->
                                                                        describeBranch "Lock more combat targets."
                                                                            (if (nextOverviewEntryToLock.objectDistanceInMeters |> Result.withDefault 999999) < context.eventContext.appSettings.targetingRange then
                                                                                describeBranch
                                                                                    "I see an overview combat entry in range to lock."
                                                                                    (lockTargetFromOverviewEntryCtrl
                                                                                        nextOverviewEntryToLock
                                                                                    )

                                                                             else
                                                                                waitForProgressInGame
                                                                            )
                                                            )
                                                        )
                                                )
                                        )
                                )
                        )

             else if distanceFromLockedTarget context < 5000 then
                returnGroupDronesToBay context "gecko"
                    |> Maybe.withDefault
                        (returnGroupDronesToBay context "bouncer"
                            |> Maybe.withDefault
                                (returnGroupDronesToBay context "warden"
                                    |> Maybe.withDefault
                                        (returnGroupDronesToBay context "garde"
                                            |> Maybe.withDefault
                                                (launchAndEngageGroupNameDrones context.readingFromGameClient context "-5000"
                                                    |> Maybe.withDefault
                                                        (describeBranch "No idling drones."
                                                            (if context.eventContext.appSettings.maxTargetCount <= (context.readingFromGameClient.targets |> List.length) then
                                                                describeBranch "Enough locked combat targets." waitForProgressInGame

                                                             else
                                                                case overviewEntriesToLock of
                                                                    [] ->
                                                                        describeBranch "I see no more overview combat entries to lock." waitForProgressInGame

                                                                    nextOverviewEntryToLock :: _ ->
                                                                        describeBranch "Lock more combat targets."
                                                                            (if (nextOverviewEntryToLock.objectDistanceInMeters |> Result.withDefault 999999) < context.eventContext.appSettings.targetingRange then
                                                                                describeBranch
                                                                                    "I see an overview combat entry in range to lock."
                                                                                    (lockTargetFromOverviewEntryCtrl
                                                                                        nextOverviewEntryToLock
                                                                                    )

                                                                             else
                                                                                waitForProgressInGame
                                                                            )
                                                            )
                                                        )
                                                )
                                        )
                                )
                        )

             else
                returnDronesToBay context context.readingFromGameClient
                    |> Maybe.withDefault
                        (returnMTUFromSpace context
                            |> Maybe.withDefault
                                waitForProgressInGame
                        )
            )

    else
        describeBranch "Managing drones while not traveling."
            (if shipCurrentSpeed context > 70 then
                describeBranch "Waiting for lower speed to deploy sentries safely." waitForProgressInGame

             else if distanceFromLockedTarget context < context.eventContext.appSettings.distanceToEngageRatsWithDrones && distanceFromLockedTarget context >= 40000 then
                returnGroupDronesToBay context "hobgoblin"
                    |> Maybe.withDefault
                        (returnGroupDronesToBay context "garde"
                            |> Maybe.withDefault
                                (returnGroupDronesToBay context "bouncer"
                                    --bouncer or warden swapped them
                                    |> Maybe.withDefault
                                        (returnGroupDronesToBay context "gecko"
                                            |> Maybe.withDefault
                                                (launchAndEngageGroupNameDrones context.readingFromGameClient context "40000+"
                                                    |> Maybe.withDefault
                                                        (describeBranch "No idling drones."
                                                            (if context.eventContext.appSettings.maxTargetCount <= (context.readingFromGameClient.targets |> List.length) then
                                                                describeBranch "Enough locked combat targets." waitForProgressInGame

                                                             else
                                                                deployMTUFromInventory context
                                                                    |> Maybe.withDefault
                                                                        (case overviewEntriesToLock of
                                                                            [] ->
                                                                                describeBranch "I see no more overview combat entries to lock." waitForProgressInGame

                                                                            nextOverviewEntryToLock :: _ ->
                                                                                describeBranch "Lock more combat targets."
                                                                                    (if (nextOverviewEntryToLock.objectDistanceInMeters |> Result.withDefault 999999) < context.eventContext.appSettings.targetingRange then
                                                                                        describeBranch
                                                                                            "I see an overview combat entry in range to lock."
                                                                                            (lockTargetFromOverviewEntryCtrl
                                                                                                nextOverviewEntryToLock
                                                                                            )

                                                                                     else
                                                                                        waitForProgressInGame
                                                                                    )
                                                                        )
                                                            )
                                                        )
                                                )
                                        )
                                )
                        )

             else if (distanceFromLockedTarget context < 40000) && (distanceFromLockedTarget context >= 20000) then
                returnGroupDronesToBay context "hobgoblin"
                    |> Maybe.withDefault
                        (returnGroupDronesToBay context "bouncer"
                            |> Maybe.withDefault
                                (returnGroupDronesToBay context "warden"
                                    |> Maybe.withDefault
                                        (returnGroupDronesToBay context "gecko"
                                            |> Maybe.withDefault
                                                (launchAndEngageGroupNameDrones context.readingFromGameClient context "-40000"
                                                    |> Maybe.withDefault
                                                        (describeBranch "No idling drones."
                                                            (if context.eventContext.appSettings.maxTargetCount <= (context.readingFromGameClient.targets |> List.length) then
                                                                describeBranch "Enough locked combat targets." waitForProgressInGame

                                                             else
                                                                deployMTUFromInventory context
                                                                    |> Maybe.withDefault
                                                                        (case overviewEntriesToLock of
                                                                            [] ->
                                                                                describeBranch "I see no more overview combat entries to lock." waitForProgressInGame

                                                                            nextOverviewEntryToLock :: _ ->
                                                                                describeBranch "Lock more combat targets."
                                                                                    (if (nextOverviewEntryToLock.objectDistanceInMeters |> Result.withDefault 999999) < context.eventContext.appSettings.targetingRange then
                                                                                        describeBranch
                                                                                            "I see an overview combat entry in range to lock."
                                                                                            (lockTargetFromOverviewEntryCtrl
                                                                                                nextOverviewEntryToLock
                                                                                            )

                                                                                     else
                                                                                        waitForProgressInGame
                                                                                    )
                                                                        )
                                                            )
                                                        )
                                                )
                                        )
                                )
                        )

             else if (distanceFromLockedTarget context < 20000) && (distanceFromLockedTarget context > 5000) then
                returnGroupDronesToBay context "hobgoblin"
                    |> Maybe.withDefault
                        (returnGroupDronesToBay context "bouncer"
                            |> Maybe.withDefault
                                (returnGroupDronesToBay context "warden"
                                    |> Maybe.withDefault
                                        (returnGroupDronesToBay context "garde"
                                            |> Maybe.withDefault
                                                (launchAndEngageGroupNameDrones context.readingFromGameClient context "gecko"
                                                    |> Maybe.withDefault
                                                        (describeBranch "No idling drones."
                                                            (if context.eventContext.appSettings.maxTargetCount <= (context.readingFromGameClient.targets |> List.length) then
                                                                describeBranch "Enough locked combat targets." waitForProgressInGame

                                                             else
                                                                deployMTUFromInventory context
                                                                    |> Maybe.withDefault
                                                                        (case overviewEntriesToLock of
                                                                            [] ->
                                                                                describeBranch "I see no more overview combat entries to lock." waitForProgressInGame

                                                                            nextOverviewEntryToLock :: _ ->
                                                                                describeBranch "Lock more combat targets."
                                                                                    (if (nextOverviewEntryToLock.objectDistanceInMeters |> Result.withDefault 999999) < context.eventContext.appSettings.targetingRange then
                                                                                        describeBranch
                                                                                            "I see an overview combat entry in range to lock."
                                                                                            (lockTargetFromOverviewEntryCtrl
                                                                                                nextOverviewEntryToLock
                                                                                            )

                                                                                     else
                                                                                        waitForProgressInGame
                                                                                    )
                                                                        )
                                                            )
                                                        )
                                                )
                                        )
                                )
                        )

             else
                returnGroupDronesToBay context "bouncer"
                    |> Maybe.withDefault
                        (returnGroupDronesToBay context "garde"
                            |> Maybe.withDefault
                                (returnGroupDronesToBay context "warden"
                                    |> Maybe.withDefault
                                        (returnGroupDronesToBay context "gecko"
                                            |> Maybe.withDefault
                                                (launchAndEngageGroupNameDrones context.readingFromGameClient context "-5000"
                                                    |> Maybe.withDefault
                                                        (describeBranch "No idling drones."
                                                            (if context.eventContext.appSettings.maxTargetCount <= (context.readingFromGameClient.targets |> List.length) then
                                                                describeBranch "Enough locked combat targets." waitForProgressInGame

                                                             else
                                                                deployMTUFromInventory context
                                                                    |> Maybe.withDefault
                                                                        (case overviewEntriesToLock of
                                                                            [] ->
                                                                                describeBranch "I see no more overview combat entries to lock." waitForProgressInGame

                                                                            nextOverviewEntryToLock :: _ ->
                                                                                describeBranch "Lock more combat targets."
                                                                                    (if (nextOverviewEntryToLock.objectDistanceInMeters |> Result.withDefault 999999) < context.eventContext.appSettings.targetingRange then
                                                                                        describeBranch
                                                                                            "I see an overview combat entry in range to lock."
                                                                                            (lockTargetFromOverviewEntryCtrl
                                                                                                nextOverviewEntryToLock
                                                                                            )

                                                                                     else
                                                                                        waitForProgressInGame
                                                                                    )
                                                                        )
                                                            )
                                                        )
                                                )
                                        )
                                )
                        )
            )

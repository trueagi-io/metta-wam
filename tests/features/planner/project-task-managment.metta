(: manageProjectTasksProcess (-> (Before ProjectState) (List Task) (After ProjectState) Report)
(:> Report (List Log))

(= (manageProjectTasksProcess $ProjectState $Tasks UpdatedProjectState) Results)
  ; Preconditions for adding tasks with dependencies
  (type-requirement (taskValid $Task) (dependenciesResolved $Task $ProjectState))
  ; Preconditions for task assignment
  (type-requirement (taskAssignable $Task) (resourceAvailable $Task $ProjectState))
  ; Preconditions for updating project state
  (type-requirement (projectStateUpdated $UpdatedProjectState $Task))
  ; Preconditions for updating stakeholders about project status
  (type-requirement (stakeHoldersUpdated $UpdatedProjectState $Task))
  ; Preconditions for logging task management activity
  (type-requirement (taskManagementLogged $Task))

  ; Manage task under those conditions
  (foreach $Task in $Tasks
    (== $UpdatedProjectState (assignTask $ProjectState $Task))
    ; Resolve dependencies for each task
    (resolveDependencies $Task)
    ; Update project state after task assignment and dependency resolution
    (== $FinalProjectState (updateProjectState $UpdatedProjectState $Task))
    ; Update stakeholders after each significant project update
    (updateStakeHolders $FinalProjectState $Task)
    ; Log activity for each managed task
    (== $Now (getCurrentTimestamp))
    (== $LogResult (logTaskManagementActivity $Task $Now))
    (addToSet $LogResults $LogResult)
  )
)

(: assignTask (-> ProjectState Task Resource ProjectState))
(: resolveDependencies (-> Task DependencyReport))
(: updateProjectState (-> ProjectState Task Resource))
(: updateStakeHolders (-> ProjectState Task))
(: logTaskManagementActivity (-> Task Resource Timestamp))
(=  (assignTask $ProjectState $Task $Resource $NewProjectState)
    (taskValid $Task)                              ; Precondition: task must meet validity checks.
    (dependenciesResolved $Task $Resource)         ; Precondition: all dependencies must be resolved.
    ; The assignTask action conforms to the above constraints.
(=  (resolveDependencies $Task $Report)
    (dependenciesSatisfiable $Report)              ; Precondition: dependencies must be satisfiable.
    (resourceAllocatable $Task))                   ; Precondition: necessary resources must be allocatable.
    ; The resolveDependencies action conforms to the above constraints.
(=  (updateProjectState $ProjectState $Task $Resource)
    (projectStateUpdated $ProjectState $Task $Resource)) ; Precondition: project state must update correctly.
    ; The updateProjectState action conforms to the above constraint.
(=  (updateStakeHolders $ProjectState $Task)
    (stakeHoldersUpdated $ProjectState $Task))     ; Precondition: stakeholders must be updated about project changes.
    ; The updateStakeHolders action conforms to the above constraint.
(=  (logTaskManagementActivity $Task $Resource $Now)
    (taskManagementLogged $Task $Resource $Now))       ; Precondition: task management action must be logged.
    ; The logTaskManagementActivity action conforms to the above constraint.




;; example PDDL
(define (domain project-management)
  (:requirements :strips :typing :equality)
  (:types task project-state log - object)

  ;; Predicates reflecting conditions and state changes
  (:predicates
    (task-valid ?t - task)
    (dependencies-resolved ?t - task ?ps - project-state)
    (task-assignable ?t - task ?ps - project-state)
    (resource-available ?t - task ?ps - project-state)
    (project-state-updated ?ps - project-state ?t - task)
    (stakeholders-updated ?ps - project-state ?t - task)
    (task-management-logged ?t - task)
  )

  ;; Actions corresponding to functions in the pseudo-functional style
  (:action assign-task
    :parameters (?ps - project-state ?t - task)
    :precondition (and (task-valid ?t) (dependencies-resolved ?t ?ps) (task-assignable ?t ?ps))
    :effect (project-state-updated ?ps ?t)
  )

  (:action resolve-dependencies
    :parameters (?t - task)
    :effect (dependencies-resolved ?t ?ps)
  )

  (:action update-project-state
    :parameters (?ps - project-state ?t - task)
    :precondition (project-state-updated ?ps ?t)
    :effect (project-state-updated ?ps ?t)
  )

  (:action update-stakeholders
    :parameters (?ps - project-state ?t - task)
    :precondition (project-state-updated ?ps ?t)
    :effect (stakeholders-updated ?ps ?t)
  )

  (:action log-task-management-activity
    :parameters (?t - task ?now - log)
    :effect (task-management-logged ?t)
  )
)


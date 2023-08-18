create_projects_mut_chunk <- "
catalogMultiCreateProjects(input: {}) {
  clientMutationId
}
"

delete_projects_mut_chunk <- "
catalogMultiDeleteProjects(input: {}) {
  clientMutationId
}
"

account_mut_template <- "
mutation AccountUpdate {
  placeholder
}
"

create_experiments_mut_chunk <- "
catalogMultiCreateExperiments(input: {}) {
  clientMutationId
}
"

delete_experiments_mut_chunk <- "
catalogMultiDeleteExperiments(input: {}) {
  clientMutationId
}
"

project_mut_template <- "
mutation ProjectUpdate {
  placeholder
}
"

upsert_samples_mut_chunk <- "
catalogMultiUpsertSamples(input: {}) {
  clientMutationId
}
"

delete_samples_mut_chunk <- "
catalogMultiDeleteSampleByName(input: {}) {
  clientMutationId
}
"

upsert_columns_mut_chunk <- "
catalogExperimentColumnDefinitionMultiUpsert(input: {}) {
  clientMutationId
}
"

table_mut_template <- "
mutation TableUpdate {
  placeholder
}
"

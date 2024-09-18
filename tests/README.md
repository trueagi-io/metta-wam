# Examples README

The `examples` directory houses a variety of sub-directories, each focusing on specific functionalities, tests, or demonstrations. These sub-directories serve as a comprehensive resource for understanding the system.

---




- **[`flybase`](./flybase)**: Focused on the Flybase module, the examples here are designed to provide insights into how this specific component works and can be used.
  - **[`extra`](./flybase/extra)**: Additional Flybase examples and tests.
    - [`nodes_med.metta`](./flybase/extra/nodes_med.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/extra/nodes_med.metta.html): Medium-sized node examples.
    - [`nodes_sm.metta`](./flybase/extra/nodes_sm.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/extra/nodes_sm.metta.html): Small node examples.
    - [`pmquery.metta`](./flybase/extra/pmquery.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/extra/pmquery.metta.html): PM query examples.
    - [`proofexample.metta`](./flybase/extra/proofexample.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/extra/proofexample.metta.html): Proof examples.
  - **[`from_das`](./flybase/from_das)**: Showcases the integration of Flybase with Distributed Atomspace (DAS).
    - [`flybase_rust_uses_python_das.metta`](./flybase/from_das/flybase_rust_uses_python_das.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/from_das/flybase_rust_uses_python_das.metta.html): Demonstrates how Flybase utilizes Python and Rust to interact with DAS.
    - [`mettalog_das_client.py`](./flybase/from_das/mettalog_das_client.py): Python client for DAS integration.
    - [`mettalog_inserts_to_das.metta`](./flybase/from_das/mettalog_inserts_to_das.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/from_das/mettalog_inserts_to_das.metta.html): Inserts data into DAS.
    - [`mettalog_uses_das.metta`](./flybase/from_das/mettalog_uses_das.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/from_das/mettalog_uses_das.metta.html): Uses DAS for data processing.
    - [`script_example_BIO.py`](./flybase/from_das/script_example_BIO.py): Example script for biological data processing.
  - **[`from_rust`](./flybase/from_rust)**: Features Flybase loaded exclusively in Rust.
    - [`flybase_rust_only.metta`](./flybase/from_rust/flybase_rust_only.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/from_rust/flybase_rust_only.metta.html): Illustrates the pure Rust implementation of Flybase.
  - **[`from_vspace`](./flybase/from_vspace)**: Using Rust to access Flybase with VSPACE.
    - [`flybase_rust_uses_python_vspace.metta`](./flybase/from_vspace/flybase_rust_uses_python_vspace.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/from_vspace/flybase_rust_uses_python_vspace.metta.html): Showcases Flybase leveraging Python VSPACE within a Rust environment.
   - [`flybase-deduced-connections.metta`](./flybase/flybase-deduced-connections.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/flybase-deduced-connections.metta.html): Mapping and connections deduced in Flybase.
   - [`flybase-deduced-queries.metta`](./flybase/flybase-deduced-queries.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/flybase-deduced-queries.metta.html): Contains queries deduced from Flybase data analysis.
   - [`flybase-deduced-types.metta`](./flybase/flybase-deduced-types.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/flybase-deduced-types.metta.html): Type definitions and structures deduced in Flybase.
   - [`flybase-loader-size-estimates.metta`](./flybase/flybase-loader-size-estimates.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/flybase-loader-size-estimates.metta.html): Size estimates and optimizations for the Flybase loader.
   - [`flybase-loader.metta`](./flybase/flybase-loader.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/flybase-loader.metta.html): Loader script for Flybase datasets.
   - [`flybase-vspace.metta`](./flybase/flybase-vspace.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/flybase-vspace.metta.html): Virtual space configuration for Flybase.
   - [`flybase-new-feature.metta`](./flybase/flybase-new-feature.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/flybase-new-feature.metta.html): New feature demonstration for Flybase.
  - **[`output~`](./flybase/output~)**: Output files and processed data from Flybase.
    - [`flybase-deduced.metta`](./flybase/output~/flybase-deduced.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/output~/flybase-deduced.metta.html): Deduced results and insights from Flybase data.
    - [`flybase-mined-flat.metta`](./flybase/output~/flybase-mined-flat.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/output~/flybase-mined-flat.metta.html): Flat structure representation of mined data in Flybase.
    - [`flybase-mined.metta`](./flybase/output~/flybase-mined.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/output~/flybase-mined.metta.html): Original mined data from Flybase.
  - **[`sanity`](./flybase/sanity)**: Sanity checks and basic tests for Flybase module.
    - [`download_file.metta`](./flybase/sanity/download_file.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/sanity/download_file.metta.html): Tests downloading a remote version of Flybase.
    - [`load_all_of_flybase.metta`](./flybase/sanity/load_all_of_flybase.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/sanity/load_all_of_flybase.metta.html): Demonstrates loading all 54 million atoms in under 30 seconds.
    - [`simple_query1.metta`](./flybase/sanity/simple_query1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/sanity/simple_query1.metta.html): Queries using the loaded atoms that complete in under a second.
    - [`simple_query2_llm.metta`](./flybase/sanity/simple_query2_llm.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/sanity/simple_query2_llm.metta.html): Combined queries with large language models.

### More Anti-Regression

- **[`more-anti-regression/chaining`](./more-anti-regression/chaining/):** Chaining examples and tests
  - [`backward_chain.metta`](./more-anti-regression/chaining/backward_chain.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/backward_chain.metta.html): Backward chaining logic.
  - [`bc-xp.metta`](./more-anti-regression/chaining/bc-xp.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/bc-xp.metta.html): Experimental backward chaining.
  - [`bc_comp_1.metta`](./more-anti-regression/chaining/bc_comp_1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/bc_comp_1.metta.html): Backward chaining compilation example 1.
  - [`bc_comp_2.metta`](./more-anti-regression/chaining/bc_comp_2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/bc_comp_2.metta.html): Backward chaining compilation example 2.
  - [`chaining_prelim.metta`](./more-anti-regression/chaining/chaining_prelim.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/chaining_prelim.metta.html): Preliminary chaining logic.
  - [`go_rel.metta`](./more-anti-regression/chaining/go_rel.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/go_rel.metta.html): Relation-based chaining logic.
  - [`parent_go.metta`](./more-anti-regression/chaining/parent_go.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/parent_go.metta.html): Parent-go chaining logic.
  - [`parent_go_1.metta`](./more-anti-regression/chaining/parent_go_1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/parent_go_1.metta.html): Parent-go chaining logic example 1.
  - [`parent_go_1_comp.metta`](./more-anti-regression/chaining/parent_go_1_comp.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/parent_go_1_comp.metta.html): Compiled parent-go chaining logic example 1.
  - [`parent_go_comp.metta`](./more-anti-regression/chaining/parent_go_comp.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/parent_go_comp.metta.html): Compiled parent-go chaining logic.
  - [`pln-xp-local_v2.metta`](./more-anti-regression/chaining/pln-xp-local_v2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/pln-xp-local_v2.metta.html): Local PLN chaining experiment version 2.
  - [`pln-xp.metta`](./more-anti-regression/chaining/pln-xp.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/pln-xp.metta.html): PLN chaining experiment.
  - [`pln_stv.metta`](./more-anti-regression/chaining/pln_stv.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/pln_stv.metta.html): PLN short-term visit chaining logic.
  - [`rules.metta`](./more-anti-regression/chaining/rules.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/rules.metta.html): Chaining rules.
  - [`rules_v2.metta`](./more-anti-regression/chaining/rules_v2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/rules_v2.metta.html): Chaining rules version 2.
  - [`sample_kb.metta`](./more-anti-regression/chaining/sample_kb.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/sample_kb.metta.html): Sample knowledge base for chaining.
  - [`sample_kb_v2.metta`](./more-anti-regression/chaining/sample_kb_v2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/sample_kb_v2.metta.html): Sample knowledge base version 2 for chaining.
  - [`sample_kb_v2_coexpressed_both.metta`](./more-anti-regression/chaining/sample_kb_v2_coexpressed_both.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/sample_kb_v2_coexpressed_both.metta.html): Coexpressed knowledge base version 2 for chaining.
  - [`sample_kb_v2_coexpressed_once.metta`](./more-anti-regression/chaining/sample_kb_v2_coexpressed_once.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/sample_kb_v2_coexpressed_once.metta.html): Coexpressed once knowledge base version 2 for chaining.
  - [`sample_kb_v2_tiny.metta`](./more-anti-regression/chaining/sample_kb_v2_tiny.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/sample_kb_v2_tiny.metta.html): Tiny knowledge base version 2 for chaining.
  - [`tadmap_edges.metta`](./more-anti-regression/chaining/tadmap_edges.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/tadmap_edges.metta.html): TAD map edges for chaining.
  - [`type_prop.metta`](./more-anti-regression/chaining/type_prop.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/type_prop.metta.html): Type properties for chaining.
  - [`types_prelim.metta`](./more-anti-regression/chaining/types_prelim.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/types_prelim.metta.html): Preliminary types for chaining.
  - [`util.py`](./more-anti-regression/chaining/util.py): Utility functions for chaining.
  - [`v2-pln-xp-easy-impl.metta`](./more-anti-regression/chaining/v2-pln-xp-easy-impl.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/v2-pln-xp-easy-impl.metta.html): Easy implementation of PLN chaining experiment version 2.
  - [`v2-pln-xp-sys-impl.metta`](./more-anti-regression/chaining/v2-pln-xp-sys-impl.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/v2-pln-xp-sys-impl.metta.html): System implementation of PLN chaining experiment version 2.
  - [`v2-pln-xp.metta`](./more-anti-regression/chaining/v2-pln-xp.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/v2-pln-xp.metta.html): PLN chaining experiment version 2.

- **[`more-anti-regression/constraint`](./more-anti-regression/constraint/):** Constraint logic examples
  - [`types.metta`](./more-anti-regression/constraint/types.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/constraint/types.metta.html): Type constraints and logic.

- **[`more-anti-regression/introspect`](./more-anti-regression/introspect/):** Introspection examples
  - [`exam-spaces.metta`](./more-anti-regression/introspect/exam-spaces.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/introspect/exam-spaces.metta.html): Examining spaces logic.
  - [`show-space-self.metta`](./more-anti-regression/introspect/show-space-self.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/introspect/show-space-self.metta.html): Self space display logic.
  - [`show-space.metta`](./more-anti-regression/introspect/show-space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/introspect/show-space.metta.html): Space display logic.
  - [`show-type.metta`](./more-anti-regression/introspect/show-type.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/introspect/show-type.metta.html): Type display logic.

- **[`more-anti-regression/minimal-metta`](./more-anti-regression/minimal-metta/):** Minimal Metta examples
  - [`stdlib_minimal.metta`](./more-anti-regression/minimal-metta/stdlib_minimal.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/minimal-metta/stdlib_minimal.metta.html): Minimal standard library.
  - [`stdlib_minimal_test.metta`](./more-anti-regression/minimal-metta/stdlib_minimal_test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/minimal-metta/stdlib_minimal_test.metta.html): Tests for minimal standard library.

- **[`more-anti-regression/spaces`](./more-anti-regression/spaces/):** Space manipulation examples
  - [`add-remove-match-float.metta`](./more-anti-regression/spaces/add-remove-match-float.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-float.metta.html): Float space manipulation.
  - [`add-remove-match-integer.metta`](./more-anti-regression/spaces/add-remove-match-integer.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-integer.metta.html): Integer space manipulation.
  - [`add-remove-match-mix-float-integer.metta`](./more-anti-regression/spaces/add-remove-match-mix-float-integer.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-mix-float-integer.metta.html): Mixed float and integer space manipulation.
  - [`add-remove-match-s-strings.metta`](./more-anti-regression/spaces/add-remove-match-s-strings.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-s-strings.metta.html): Single string space manipulation.
  - [`add-remove-match-s-symbols.metta`](./more-anti-regression/spaces/add-remove-match-s-symbols.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-s-symbols.metta.html): Single symbol space manipulation.
  - [`add-remove-match-ss-strings.metta`](./more-anti-regression/spaces/add-remove-match-ss-strings.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-ss-strings.metta.html): Double string space manipulation.
  - [`add-remove-match-ss-symbols.metta`](./more-anti-regression/spaces/add-remove-match-ss-symbols.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-ss-symbols.metta.html): Double symbol space manipulation.
  - [`add-remove-match-strings.metta`](./more-anti-regression/spaces/add-remove-match-strings.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-strings.metta.html): String space manipulation.
  - [`add-remove-match-symbols.metta`](./more-anti-regression/spaces/add-remove-match-symbols.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-symbols.metta.html): Symbol space manipulation.

- **[`more-anti-regression/std`](./more-anti-regression/std/):** Standard examples
  - [`animals.metta`](./more-anti-regression/std/animals.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/animals.metta.html): Animal logic examples.
  - [`example_pm_queries.metta`](./more-anti-regression/std/example_pm_queries.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/example_pm_queries.metta.html): Example PM queries.
  - [`gadt.metta`](./more-anti-regression/std/gadt.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/gadt.metta.html): Generalized algebraic data types.
  - [`get-atoms-test.metta`](./more-anti-regression/std/get-atoms-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/get-atoms-test.metta.html): Atom retrieval test.
  - [`grounded_basic.metta`](./more-anti-regression/std/grounded_basic.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/grounded_basic.metta.html): Basic grounded logic.
  - [`higher_order_funcs.metta`](./more-anti-regression/std/higher_order_funcs.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/higher_order_funcs.metta.html): Higher-order functions.
  - [`kb_write.metta`](./more-anti-regression/std/kb_write.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/kb_write.metta.html): Knowledge base writing.
  - [`lte.metta`](./more-anti-regression/std/lte.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/lte.metta.html): Less than or equal logic.
  - [`nondeterm.metta`](./more-anti-regression/std/nondeterm.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/nondeterm.metta.html): Non-deterministic logic.
  - [`spaces_kb.metta`](./more-anti-regression/std/spaces_kb.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/spaces_kb.metta.html): Spaces knowledge base.
  - [`symbols.metta`](./more-anti-regression/std/symbols.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/symbols.metta.html): Symbol logic examples.

- **[`more-anti-regression/stdlib-mettalog`](./more-anti-regression/stdlib-mettalog/):** MettaLog standard library
  - [`interpreter_minimal.rs.metta`](./more-anti-regression/stdlib-mettalog/interpreter_minimal.rs.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/stdlib-mettalog/interpreter_minimal.rs.metta.html): Minimal interpreter.
  - [`stdlib.rs.metta`](./more-anti-regression/stdlib-mettalog/stdlib.rs.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/stdlib-mettalog/stdlib.rs.metta.html): Standard library for MettaLog.
  - [`stdlib_mettalog_test.metta`](./more-anti-regression/stdlib-mettalog/stdlib_mettalog_test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/stdlib-mettalog/stdlib_mettalog_test.metta.html): Tests for MettaLog standard library.
  - [`stdlib_mettalog_test_pt2.metta`](./more-anti-regression/stdlib-mettalog/stdlib_mettalog_test_pt2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/stdlib-mettalog/stdlib_mettalog_test_pt2.metta.html): Part 2 of tests for MettaLog standard library.

### More Anti-Regression

- **[`more-anti-regression/chaining`](./more-anti-regression/chaining/):** Chaining examples and tests
  - [`backward_chain.metta`](./more-anti-regression/chaining/backward_chain.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/backward_chain.metta.html): Backward chaining logic.
  - [`bc-xp.metta`](./more-anti-regression/chaining/bc-xp.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/bc-xp.metta.html): Experimental backward chaining.
  - [`bc_comp_1.metta`](./more-anti-regression/chaining/bc_comp_1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/bc_comp_1.metta.html): Backward chaining compilation example 1.
  - [`bc_comp_2.metta`](./more-anti-regression/chaining/bc_comp_2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/chaining/bc_comp_2.metta.html): Backward chaining compilation example 2.

- **[`more-anti-regression/constraint`](./more-anti-regression/constraint/):** Constraint logic examples
  - [`types.metta`](./more-anti-regression/constraint/types.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/constraint/types.metta.html): Type constraints and logic.

- **[`more-anti-regression/introspect`](./more-anti-regression/introspect/):** Introspection examples
  - [`exam-spaces.metta`](./more-anti-regression/introspect/exam-spaces.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/introspect/exam-spaces.metta.html): Examining spaces logic.
  - [`show-space-self.metta`](./more-anti-regression/introspect/show-space-self.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/introspect/show-space-self.metta.html): Self space display logic.
  - [`show-space.metta`](./more-anti-regression/introspect/show-space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/introspect/show-space.metta.html): Space display logic.
  - [`show-type.metta`](./more-anti-regression/introspect/show-type.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/introspect/show-type.metta.html): Type display logic.

- **[`more-anti-regression/spaces`](./more-anti-regression/spaces/):** Space manipulation examples
  - [`add-remove-match-float.metta`](./more-anti-regression/spaces/add-remove-match-float.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-float.metta.html): Float space manipulation.
  - [`add-remove-match-integer.metta`](./more-anti-regression/spaces/add-remove-match-integer.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/spaces/add-remove-match-integer.metta.html): Integer space manipulation.

- **[`more-anti-regression/std`](./more-anti-regression/std/):** Standard examples
  - [`animals.metta`](./more-anti-regression/std/animals.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/animals.metta.html): Animal logic examples.
  - [`example_pm_queries.metta`](./more-anti-regression/std/example_pm_queries.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/std/example_pm_queries.metta.html): Example PM queries.

- **[`more-anti-regression/stdlib-mettalog`](./more-anti-regression/stdlib-mettalog/):** MettaLog standard library
  - [`interpreter_minimal.rs.metta`](./more-anti-regression/stdlib-mettalog/interpreter_minimal.rs.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/stdlib-mettalog/interpreter_minimal.rs.metta.html): Minimal interpreter.
  - [`stdlib.rs.metta`](./more-anti-regression/stdlib-mettalog/stdlib.rs.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/more-anti-regression/stdlib-mettalog/stdlib.rs.metta.html): Standard library for MettaLog.

#### **Flybase with Distributed Atomspace (DAS) Integration**
- **[`flybase/from_das`](././flybase/from_das)**: Showcases the integration of Flybase with Distributed Atomspace (DAS), a system for managing and processing distributed atom-based data.
  - [`flybase_rust_uses_python_das.metta`](././flybase/from_das/flybase_rust_uses_python_das.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/from_das/flybase_rust_uses_python_das.metta.html): Demonstrates how Flybase utilizes Python and Rust to interact with DAS, enhancing its data analysis and processing capabilities.

#### **Flybase with Rust-Only Implementation**
- **[`flybase/from_rust`](././flybase/from_rust)**: Features Flybase loaded exclusively in Rust
  - [`flybase_rust_only.metta`](././flybase/from_rust/flybase_rust_only.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/from_rust/flybase_rust_only.metta.html): Illustrates the pure Rust implementation of Flybase, highlighting its efficiency and direct integration with system-level functionalities.

#### **Flybase with VSPACE (Version Space) Integration**
- **[`flybase/from_vspace`](././flybase/from_vspace)**: Using Rust to access Flybase with VSPACE ( hosting 56 million atoms )
  - [`flybase_rust_uses_python_vspace.metta`](././flybase/from_vspace/flybase_rust_uses_python_vspace.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/flybase/from_vspace/flybase_rust_uses_python_vspace.metta.html): Showcases Flybase leveraging Python VSPACE within a Rust environment, focusing on efficient handling and manipulation of a vast space containing millions of atoms.


## Hyperon Miner Examples

- **[`new_directory`](./new_directory/):** New directory for additional examples
    - [`example1.metta`](./new_directory/example1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/new_directory/example1.metta.html): Example 1 for new directory.
    - [`example2.metta`](./new_directory/example2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/new_directory/example2.metta.html): Example 2 for new directory.

- **[`extended_compat/hyperon-miner/`](./extended_compat/hyperon-miner/):** Hyperon miner directory
    - [`data/sample.metta`](./extended_compat/hyperon-miner/data/sample.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/data/sample.metta.html): Sample data for Hyperon miner testing.
    - [`utils/MinerUtils.metta`](./extended_compat/hyperon-miner/utils/MinerUtils.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/utils/MinerUtils.metta.html): Utility functions for Hyperon mining operations.
   - [`match/MinerMatchTest.metta`](./extended_compat/hyperon-miner/match/MinerMatchTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/match/MinerMatchTest.metta.html): Test cases for Hyperon miner matching algorithms.
   - [`match/MinerMatch.metta`](./extended_compat/hyperon-miner/match/MinerMatch.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/match/MinerMatch.metta.html): Implementation of matching algorithms for Hyperon miner.
   - **[`dependent-types`](././extended_compat/hyperon-miner/dependent-types)**: Dependent types in Hyperon miner.
     - [`MinerCurriedDTL.metta`](./extended_compat/hyperon-miner/dependent-types/MinerCurriedDTL.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerCurriedDTL.metta.html): Curried dependent type logic.
     - [`MinerCurriedDTLTest.metta`](./extended_compat/hyperon-miner/dependent-types/MinerCurriedDTLTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerCurriedDTLTest.metta.html): Tests for curried dependent type logic.
     - [`MinerDTL.metta`](./extended_compat/hyperon-miner/dependent-types/MinerDTL.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerDTL.metta.html): Dependent type logic.
     - [`MinerDTL1.metta`](./extended_compat/hyperon-miner/dependent-types/MinerDTL1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerDTL1.metta.html): First version of dependent type logic.
     - [`MinerDTL1Test.metta`](./extended_compat/hyperon-miner/dependent-types/MinerDTL1Test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerDTL1Test.metta.html): Tests for first version of dependent type logic.
     - [`MinerDTL2.metta`](./extended_compat/hyperon-miner/dependent-types/MinerDTL2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerDTL2.metta.html): Second version of dependent type logic.
     - [`MinerDTL2Test.metta`](./extended_compat/hyperon-miner/dependent-types/MinerDTL2Test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerDTL2Test.metta.html): Tests for second version of dependent type logic.
     - [`MinerDTL3.metta`](./extended_compat/hyperon-miner/dependent-types/MinerDTL3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerDTL3.metta.html): Third version of dependent type logic.
     - [`MinerDTL3Test.metta`](./extended_compat/hyperon-miner/dependent-types/MinerDTL3Test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerDTL3Test.metta.html): Tests for third version of dependent type logic.
     - [`MinerDTLTest.metta`](./extended_compat/hyperon-miner/dependent-types/MinerDTLTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/MinerDTLTest.metta.html): General tests for dependent type logic.
     - [`SupportRuleExp.metta`](./extended_compat/hyperon-miner/dependent-types/SupportRuleExp.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/SupportRuleExp.metta.html): Support rule expressions.
     - [`bchain.metta`](./extended_compat/hyperon-miner/dependent-types/bchain.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/bchain.metta.html): Backward chaining logic.
     - [`chainer.metta`](./extended_compat/hyperon-miner/dependent-types/chainer.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/chainer.metta.html): Chaining logic.
     - [`succAxiom.metta`](./extended_compat/hyperon-miner/dependent-types/succAxiom.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-miner/dependent-types/succAxiom.metta.html): Successor axioms.






## Extended Compatibility Examples

### Combinator Logic Experiments
- **[`extended_compat/metta-examples/combinator_logic_experiments/`](./extended_compat/metta-examples/combinator_logic_experiments/):** Combinator Logic Experiments Directory
   - [`y_comb_examples.metta`](./extended_compat/metta-examples/combinator_logic_experiments/y_comb_examples.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/combinator_logic_experiments/y_comb_examples.metta.html): Examples and experiments with Y combinators.

### Logic
- **[`extended_compat/metta-examples/logic/`](./extended_compat/metta-examples/logic/):** Metta Logic Directory
   - [`puzzle.metta`](./extended_compat/metta-examples/logic/puzzle.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/logic/puzzle.metta.html): Logic puzzles and solutions in Metta.
   - [`all_any.metta`](./extended_compat/metta-examples/logic/all_any.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/logic/all_any.metta.html): Implementations of universal and existential quantifiers.
   - [`memb.metta`](./extended_compat/metta-examples/logic/memb.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/logic/memb.metta.html): Member checking functions in logical expressions.

### SICP Book Examples
- **[`extended_compat/metta-examples/SICP_book/`](./extended_compat/metta-examples/SICP_book/):** SICP Book Examples in Metta
  - [Chapter 1.1](extended_compat/metta-examples/SICP_book/chapter_1_1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/SICP_book/chapter_1_1.metta.html)
  - [Chapter 1.2](extended_compat/metta-examples/SICP_book/chapter_1_2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/SICP_book/chapter_1_2.metta.html)
  - [Chapter 1.3](extended_compat/metta-examples/SICP_book/chapter_1_3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/SICP_book/chapter_1_3.metta.html)

### Recursion Schemes
- **[`extended_compat/metta-examples/recursion-schemes/src/`](./extended_compat/metta-examples/recursion-schemes/src/):** Recursion Schemes in Metta
   - [`schemes.metta`](./extended_compat/metta-examples/recursion-schemes/src/schemes.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/recursion-schemes/src/schemes.metta.html): Various recursion schemes implemented in Metta.
   - [`base.metta`](./extended_compat/metta-examples/recursion-schemes/src/base.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/recursion-schemes/src/base.metta.html): Base functions and utilities for recursion schemes.
   - [`expression.metta`](./extended_compat/metta-examples/recursion-schemes/src/expression.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/recursion-schemes/src/expression.metta.html): Expression handling in recursion schemes.
   - [`benchmark.metta`](./extended_compat/metta-examples/recursion-schemes/src/benchmark.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/recursion-schemes/src/benchmark.metta.html): Benchmark tests for recursion scheme implementations.

### Probabilistic Dependent Types
- **[`extended_compat/metta-examples/prob-dep-types/`](./extended_compat/metta-examples/prob-dep-types/):** Probabilistic Dependent Types in Metta
   - [`prob_dep_types.metta`](./extended_compat/metta-examples/prob-dep-types/prob_dep_types.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/prob-dep-types/prob_dep_types.metta.html): Definitions and examples of probabilistic dependent types.
   - [`inf_order_probs.metta`](./extended_compat/metta-examples/prob-dep-types/inf_order_probs.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/metta-examples/prob-dep-types/inf_order_probs.metta.html): Infinite order probabilities and their applications.

## NARS (Non-Axiomatic Reasoning System)

#### **NARS - Current Version**
- **[`nars/current`](./extended_compat/nars/current)**: The current iteration of the Non-Axiomatic Reasoning System (NARS) showcasing the latest features and tests.
  - [`NARS.metta`](./extended_compat/nars/current/NARS.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/current/NARS.metta.html): Main script for the current version of NARS.
  - [`tests0.metta`](./extended_compat/nars/current/tests0.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/current/tests0.metta.html): Basic tests for the current NARS version.
  - [`tests1.metta`](./extended_compat/nars/current/tests1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/current/tests1.metta.html): Comprehensive level tests for NARS functionalities.
  - [`tests2.metta`](./extended_compat/nars/current/tests2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/current/tests2.metta.html): Advanced testing scenarios for NARS.
  - [`tests3.metta`](./extended_compat/nars/current/tests3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/current/tests3.metta.html): Tests focusing on specific reasoning capabilities in NARS.
  - [`tests4.metta`](./extended_compat/nars/current/tests4.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/current/tests4.metta.html): Specialized tests for complex reasoning in NARS.
  - [`tests5.metta`](./extended_compat/nars/current/tests5.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/current/tests5.metta.html): Simple test for complex reasoning in NARS.

#### **NARS - New Developments**
- **[`nars/new`](./extended_compat/nars/new)**: Houses the latest developments and experimental features in NARS.
  - [`NARS.metta`](./extended_compat/nars/new/NARS.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/new/NARS.metta.html)[`tests0.metta`](./extended_compat/nars/new/tests0.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/new/tests0.metta.html)[`tests1.metta`](./extended_compat/nars/new/tests1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/new/tests1.metta.html)[`tests2.metta`](./extended_compat/nars/new/tests2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/new/tests2.metta.html)[`tests3.metta`](./extended_compat/nars/new/tests3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/new/tests3.metta.html)[`tests4.metta`](./extended_compat/nars/new/tests4.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/new/tests4.metta.html)[`tests5.metta`](./extended_compat/nars/new/tests5.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/new/tests5.metta.html): Reflects ongoing experiments and cutting-edge features in NARS development.

#### **NARS Extras**
- **[`nars/nars_extras`](./extended_compat/nars/nars_extras)**: Additional scripts and utilities for NARS.
  - [`NARS_BuildTupleCounts.metta`](./extended_compat/nars/nars_extras/NARS_BuildTupleCounts.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_extras/NARS_BuildTupleCounts.metta.html): Script for building tuple counts in NARS.
  - [`RUN_minnars.metta`](./extended_compat/nars/nars_extras/RUN_minnars.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_extras/RUN_minnars.metta.html): Execution script for MinNARS, a minimalistic version of NARS.
  - [`RUN_minnars_trimmed.metta`](./extended_compat/nars/nars_extras/RUN_minnars_trimmed.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_extras/RUN_minnars_trimmed.metta.html): Trimmed version of the MinNARS execution script.
  - [`TestNARS_listing.metta`](./extended_compat/nars/nars_extras/TestNARS_listing.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_extras/TestNARS_listing.metta.html): Comprehensive test listings and scripts for NARS.

#### **NARS - Previous Versions**
- **[`nars/prev`](./extended_compat/nars/prev)**: Includes tests and scripts from previous versions of NARS, showcasing the evolution of the system.
  - [`NARS.metta`](./extended_compat/nars/prev/NARS.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/prev/NARS.metta.html)[`tests0.metta`](./extended_compat/nars/prev/tests0.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/prev/tests0.metta.html)[`tests1.metta`](./extended_compat/nars/prev/tests1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/prev/tests1.metta.html)[`tests2.metta`](./extended_compat/nars/prev/tests2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/prev/tests2.metta.html)[`tests3.metta`](./extended_compat/nars/prev/tests3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/prev/tests3.metta.html)[`tests4.metta`](./extended_compat/nars/prev/tests4.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/prev/tests4.metta.html)[`tests5.metta`](./extended_compat/nars/prev/tests5.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/prev/tests5.metta.html): Provides a historical perspective on the progression and updates in NARS.

#### **NARS Version 0**
- **[`nars/nars_v0`](./extended_compat/nars/nars_v0)**: The original version of NARS, preserving the initial features and tests.
  - [`NARS.metta`](./extended_compat/nars/nars_v0/NARS.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_v0/NARS.metta.html): Main script for the original NARS version.
  - [`NARS-old.metta`](./extended_compat/nars/nars_v0/NARS-old.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_v0/NARS-old.metta.html): Older version of the NARS main script.
  - [`tests0.metta`](./extended_compat/nars/nars_v0/tests0.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_v0/tests0.metta.html)[`tests1.metta`](./extended_compat/nars/nars_v0/tests1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_v0/tests1.metta.html)[`tests2.metta`](./extended_compat/nars/nars_v0/tests2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_v0/tests2.metta.html)[`tests3.metta`](./extended_compat/nars/nars_v0/tests3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_v0/tests3.metta.html)[`tests4.metta`](./extended_compat/nars/nars_v0/tests4.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_v0/tests4.metta.html)[`tests5.metta`](./extended_compat/nars/nars_v0/tests5.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/nars/nars_v0/tests5.metta.html) A series of tests ranging from basic to comprehensive for the original NARS version.




## Python Compatibility - Hyperon Experimental Python

- **[`python_compat/hyperon-experimental_python/sandbox/`](./python_compat/hyperon-experimental_python/sandbox/):** Hyperon Experimental Python Sandbox
   - [`resolve/r.metta`](./python_compat/hyperon-experimental_python/sandbox/resolve/r.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/hyperon-experimental_python/sandbox/resolve/r.metta.html): Resolution algorithms in Hyperon Python sandbox.
   - [`sql_space/sql_space_test.metta`](./python_compat/hyperon-experimental_python/sandbox/sql_space/sql_space_test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/hyperon-experimental_python/sandbox/sql_space/sql_space_test.metta.html): Test scripts for SQL space integration in Hyperon.
   - [`neurospace/test_assist.metta`](./python_compat/hyperon-experimental_python/sandbox/neurospace/test_assist.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/hyperon-experimental_python/sandbox/neurospace/test_assist.metta.html): Assistant scripts for neurospace simulations.
   - [`neurospace/test_nspace.metta`](./python_compat/hyperon-experimental_python/sandbox/neurospace/test_nspace.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/hyperon-experimental_python/sandbox/neurospace/test_nspace.metta.html): Neurospace testing and simulation scripts.
   - [`numpy/nm_test.metta`](./python_compat/hyperon-experimental_python/sandbox/numpy/nm_test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/hyperon-experimental_python/sandbox/numpy/nm_test.metta.html): Numpy integration tests in Hyperon Python environment.
   - [`das_gate/test_das.metta`](./python_compat/hyperon-experimental_python/sandbox/das_gate/test_das.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/hyperon-experimental_python/sandbox/das_gate/test_das.metta.html): Test scripts for DAS gate functionality in Hyperon.

- **[`python_compat/extend/`](./python_compat/extend/):** Metta-Morph Python Compatibility Layer for NARS
	   - [`example6.metta`](./python_compat/extend/example6.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/extend/example6.metta.html): Example script 6 for Python compatibility layer.
	   - [`compileme.metta`](./python_compat/extend/compileme.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/extend/compileme.metta.html): Compilation scripts for Python compatibility extensions.
	   - [`example2.metta`](./python_compat/extend/example2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/extend/example2.metta.html): Example script 2 for extended functionalities.
	   - [`example5.metta`](./python_compat/extend/example5.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/extend/example5.metta.html): Example script 5 showcasing extended features.
	   - [`example4.metta`](./python_compat/extend/example4.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/extend/example4.metta.html): Example script 4 demonstrating extended capabilities.
	   - [`example1.metta`](./python_compat/extend/example1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/extend/example1.metta.html): Example script 1 for Python compatibility tests.
	   - [`example3.metta`](./python_compat/extend/example3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/extend/example3.metta.html): Example script 3 in Python compatibility layer.
	   - [`timing.metta`](./python_compat/timing/timing.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/timing/timing.metta.html): Timing and performance testing scripts.
	   - [`mettamorph.metta`](./python_compat/mettamorph.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/python_compat/mettamorph.metta.html): Inital Script of MeTTa-Morph.


#### **Synthesis and Logic Group**

This group of directories explores various representational methods for porting PLN to MeTTa.
- **[`extended_compat/hyperon-pln/metta/`](./extended_compat/hyperon-pln/metta/):** Hyperon PLN Metta Base Compatibility

- **[`entail`](./extended_compat/hyperon-pln/metta/entail)**: Here, rules are expressed with the symbol `?`.
   - [`DeductionEntailTest.metta`](./extended_compat/hyperon-pln/metta/entail/DeductionEntailTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/entail/DeductionEntailTest.metta.html): Tests for deduction entailment in MeTTa.
   - [`ImplicationDirectIntroductionEntail.metta`](./extended_compat/hyperon-pln/metta/entail/ImplicationDirectIntroductionEntail.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/entail/ImplicationDirectIntroductionEntail.metta.html): Direct implication introduction entailment logic.
   - [`ImplicationDirectIntroductionEntailTest.metta`](./extended_compat/hyperon-pln/metta/entail/ImplicationDirectIntroductionEntailTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/entail/ImplicationDirectIntroductionEntailTest.metta.html): Tests for direct implication introduction entailment.
   - [`DeductionEntail.metta`](./extended_compat/hyperon-pln/metta/entail/DeductionEntail.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/entail/DeductionEntail.metta.html): Logic for deduction entailment in MeTTa.

- **[`common`](./extended_compat/hyperon-pln/metta/common)**: Contains common definitions for PLN in MeTTa.
   - [`Num.metta`](./extended_compat/hyperon-pln/metta/common/Num.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/Num.metta.html): Numerical utilities and definitions.
   - [`Record.metta`](./extended_compat/hyperon-pln/metta/common/Record.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/Record.metta.html): Record structures and related logic.
   - [`DeductionFormula.metta`](./extended_compat/hyperon-pln/metta/common/formula/DeductionFormula.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/formula/DeductionFormula.metta.html): Formulae for logical deduction.
   - [`ImplicationDirectIntroductionFormula.metta`](./extended_compat/hyperon-pln/metta/common/formula/ImplicationDirectIntroductionFormula.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/formula/ImplicationDirectIntroductionFormula.metta.html): Formulae for direct implication introduction.
   - [`ModusPonensFormula.metta`](./extended_compat/hyperon-pln/metta/common/formula/ModusPonensFormula.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/formula/ModusPonensFormula.metta.html): Formulae for modus ponens logic.
   - [`DeductionFormulaTest.metta`](./extended_compat/hyperon-pln/metta/common/formula/DeductionFormulaTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/formula/DeductionFormulaTest.metta.html): Test cases for logical deduction formulae.
   - [`OrderedSet.metta`](./extended_compat/hyperon-pln/metta/common/OrderedSet.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/OrderedSet.metta.html): Implementation of ordered sets.
   - [`EqualityType.metta`](./extended_compat/hyperon-pln/metta/common/EqualityType.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/EqualityType.metta.html): Equality types and related logic.
   - [`ListTest.metta`](./extended_compat/hyperon-pln/metta/common/ListTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/ListTest.metta.html): Tests for list operations.
   - [`NumTest.metta`](./extended_compat/hyperon-pln/metta/common/NumTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/NumTest.metta.html): Tests for numerical utilities.
   - [`OrderedSetTest.metta`](./extended_compat/hyperon-pln/metta/common/OrderedSetTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/OrderedSetTest.metta.html): Tests for ordered set implementation.
   - [`Num8.metta`](./extended_compat/hyperon-pln/metta/common/Num8.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/Num8.metta.html): Extended numerical operations.
   - [`Maybe.metta`](./extended_compat/hyperon-pln/metta/common/Maybe.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/Maybe.metta.html): Implementation of 'Maybe' monad.
   - [`MaybeTest.metta`](./extended_compat/hyperon-pln/metta/common/MaybeTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/MaybeTest.metta.html): Tests for 'Maybe' monad.
   - [`EqualityTypeTest.metta`](./extended_compat/hyperon-pln/metta/common/EqualityTypeTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/EqualityTypeTest.metta.html): Tests for equality type logic.
   - [`In.metta`](./extended_compat/hyperon-pln/metta/common/In.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/In.metta.html): Logic for element inclusion.
   - [`List.metta`](./extended_compat/hyperon-pln/metta/common/List.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/List.metta.html): List operations and utilities.
   - [`TruthValue.metta`](./extended_compat/hyperon-pln/metta/common/truthvalue/TruthValue.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/truthvalue/TruthValue.metta.html): Base structure for truth values.
   - [`EvidentialTruthValue.metta`](./extended_compat/hyperon-pln/metta/common/truthvalue/EvidentialTruthValue.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/truthvalue/EvidentialTruthValue.metta.html): Evidential truth value implementation.
   - [`EvidentialTruthValueTest.metta`](./extended_compat/hyperon-pln/metta/common/truthvalue/EvidentialTruthValueTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/truthvalue/EvidentialTruthValueTest.metta.html): Evidential truth value Tests.
   - [`TruthValueTest.metta`](./extended_compat/hyperon-pln/metta/common/truthvalue/TruthValueTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/truthvalue/TruthValueTest.metta.html): General tests for truth values.
   - [`TemporalTruthValue.metta`](./extended_compat/hyperon-pln/metta/common/truthvalue/TemporalTruthValue.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/truthvalue/TemporalTruthValue.metta.html): Temporal aspect of truth values.
   - [`MeasEq.metta`](./extended_compat/hyperon-pln/metta/common/truthvalue/MeasEq.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/truthvalue/MeasEq.metta.html): Measurement equality logic.
   - [`InTest.metta`](./extended_compat/hyperon-pln/metta/common/InTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/InTest.metta.html): Tests for element inclusion logic.
   - [`BelieveMe.metta`](./extended_compat/hyperon-pln/metta/common/BelieveMe.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/common/BelieveMe.metta.html): Logic for belief representation.

- **[`converters`](./extended_compat/hyperon-pln/metta/converters)**: Focuses on Conversion
   - [`converters/calculi-converter.metta`](./extended_compat/hyperon-pln/metta/converters/calculi-converter.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/converters/calculi-converter.metta.html): Handles specific converters between logical systems or formal calculi in the Metta environment.
   - [`curry.metta`](./extended_compat/hyperon-pln/metta/converters/curry.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/converters/curry.metta.html): Involves currying functions, transforming functions with multiple arguments into a sequence of single-argument functions.


- **[`HOL`](./extended_compat/hyperon-pln/metta/hol)**: Showcases HOL (Higher-Order Logic) operations.
   - [`ListTest.metta`](./extended_compat/hyperon-pln/metta/hol/ListTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/hol/ListTest.metta.html): Test cases for list operations in HOL.
   - [`lambda-flatten.metta`](./extended_compat/hyperon-pln/metta/hol/lambda-flatten.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/hol/lambda-flatten.metta.html): Tests involving Higher Order Lambda Calculus
   - [`calculi-converter.metta`](./extended_compat/hyperon-pln/metta/hol/calculi-converter.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/hol/calculi-converter.metta.html): Involves tests or utilities for converting between different logical calculi, focusing on higher-order logic (HOL).
   - [`NatTest.metta`](./extended_compat/hyperon-pln/metta/hol/NatTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/hol/NatTest.metta.html): Test cases for natural number operations in HOL.
   - [`NatSimpleTest.metta`](./extended_compat/hyperon-pln/metta/hol/NatSimpleTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/hol/NatSimpleTest.metta.html): Simplified tests for natural numbers in HOL.

- **[`dependent-types`](./extended_compat/hyperon-pln/metta/dependent-types)**: Concentrates on representing rules as type constructors.
   - [`ModusPonensDTLTest.metta`](./extended_compat/hyperon-pln/metta/dependent-types/ModusPonensDTLTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/dependent-types/ModusPonensDTLTest.metta.html): Test cases for modus ponens in dependent type logic.
   - [`ImplicationDirectIntroductionDTL.metta`](./extended_compat/hyperon-pln/metta/dependent-types/ImplicationDirectIntroductionDTL.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/dependent-types/ImplicationDirectIntroductionDTL.metta.html): Direct implication introduction in dependent type logic.
   - [`DeductionImplicationDirectIntroductionDTLTest.metta`](./extended_compat/hyperon-pln/metta/dependent-types/DeductionImplicationDirectIntroductionDTLTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/dependent-types/DeductionImplicationDirectIntroductionDTLTest.metta.html): Tests for deduction and direct implication introduction.
   - [`ImplicationDirectIntroductionDTLTest.metta`](./extended_compat/hyperon-pln/metta/dependent-types/ImplicationDirectIntroductionDTLTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/dependent-types/ImplicationDirectIntroductionDTLTest.metta.html): More tests for direct implication introduction.
   - [`DeductionDTL.metta`](./extended_compat/hyperon-pln/metta/dependent-types/DeductionDTL.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/dependent-types/DeductionDTL.metta.html): Deduction logic in dependent type language.
   - [`DeductionDTLTest.metta`](./extended_compat/hyperon-pln/metta/dependent-types/DeductionDTLTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/dependent-types/DeductionDTLTest.metta.html): Tests for deduction in dependent type language.
   - [`ModusPonensDTL.metta`](./extended_compat/hyperon-pln/metta/dependent-types/ModusPonensDTL.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/dependent-types/ModusPonensDTL.metta.html): Modus ponens implementation in dependent type language.

- **[`forward-chainer`](./extended_compat/hyperon-pln/metta/forward-chainer)**: Focuses on forward-chaining logic.
   - [`forward-chainer-test.metta`](./extended_compat/hyperon-pln/metta/forward-chainer/forward-chainer-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/forward-chainer/forward-chainer-test.metta.html): Test cases for forward-chaining logic.
   - [`forward-chainer-xp.metta`](./extended_compat/hyperon-pln/metta/forward-chainer/forward-chainer-xp.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/forward-chainer/forward-chainer-xp.metta.html): Experimental forward-chaining implementations.


- **[`program-verification`](./extended_compat/hyperon-pln/metta/program-verification)**: Focuses on Program Verification
   - [`ListTest.metta`](./extended_compat/hyperon-pln/metta/program-verification/ListTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/program-verification/ListTest.metta.html): Verifies properties related to list structures in a program verification context.
   - [`NatDTLTest.metta`](./extended_compat/hyperon-pln/metta/program-verification/NatDTLTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/program-verification/NatDTLTest.metta.html): Tests properties of natural numbers within a decision table logic (DTL) context.
   - [`NatParityTest.metta`](./extended_compat/hyperon-pln/metta/program-verification/NatParityTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/program-verification/NatParityTest.metta.html): Tests natural number parity (even/odd properties) within a program verification framework.
   - [`NatSimpleTest.metta`](./extended_compat/hyperon-pln/metta/program-verification/NatSimpleTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/program-verification/NatSimpleTest.metta.html): Tests basic properties of natural numbers in program verification.
   - [`NatStandaloneTest.metta`](./extended_compat/hyperon-pln/metta/program-verification/NatStandaloneTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/program-verification/NatStandaloneTest.metta.html): Tests natural numbers as standalone entities, without dependencies on other verification systems.
   - [`NatTest.metta`](./extended_compat/hyperon-pln/metta/program-verification/NatTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/program-verification/NatTest.metta.html): Tests general properties of natural numbers in a program verification context.

- **[`proof-trees`](./extended_compat/hyperon-pln/metta/proof-trees)**: Focuses on Proof Trees
   - [`bug.metta`](./extended_compat/hyperon-pln/metta/proof-tree/bug.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/proof-tree/bug.metta.html): Involves identifying or testing bugs in the proof tree system.
   - [`prg-assistant-example.metta`](./extended_compat/hyperon-pln/metta/proof-tree/prg-assistant-example.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/proof-tree/prg-assistant-example.metta.html): Example for using the proof assistant feature with proof trees.
   - [`proof-tree.metta`](./extended_compat/hyperon-pln/metta/proof-tree/proof-tree.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/proof-tree/proof-tree.metta.html): Core implementation or tests for proof trees in logic, supporting formal verification and reasoning processes.


- **[`subtyping`](./extended_compat/hyperon-pln/metta/subtyping)**: Showcases subtyping operations.
   - [`rule-base.metta`](./extended_compat/hyperon-pln/metta/subtyping/rule-base.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/subtyping/rule-base.metta.html): Base rules for subtyping operations.
   - [`subtyping-test.metta`](./extended_compat/hyperon-pln/metta/subtyping/subtyping-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/subtyping/subtyping-test.metta.html): Test cases for subtyping.

- **[`sumo`](./extended_compat/hyperon-pln/metta/sumo)**: Houses SUMO (Suggested Upper Merged Ontology) examples.
    - [`load-suo-kif.metta`](./extended_compat/hyperon-pln/metta/sumo/load-suo-kif.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/sumo/load-suo-kif.metta.html): Logic for loading SUMO KIF files.
    - [`rule-base.metta`](./extended_compat/hyperon-pln/metta/sumo/rule-base.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/sumo/rule-base.metta.html): Base rules for SUMO logic.
   - **[`sumo/john-carry-flower`](././extended_compat/hyperon-pln/metta/sumo/john-carry-flower)**: Narrative logic examples.
      - [`john-carry-flower-test.metta`](./extended_compat/hyperon-pln/metta/sumo/john-carry-flower/john-carry-flower-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/sumo/john-carry-flower/john-carry-flower-test.metta.html): Test cases for the John carrying a flower narrative.
      - [`john-carry-flower.kif.metta`](./extended_compat/hyperon-pln/metta/sumo/john-carry-flower/john-carry-flower.kif.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/sumo/john-carry-flower/john-carry-flower.kif.metta.html): KIF narrative for John carrying a flower.
    - **[`sumo/orientation`](././extended_compat/hyperon-pln/metta/sumo/orientation)**: Related to spatial orientation.
      - [`orientation-test.metta`](./extended_compat/hyperon-pln/metta/sumo/orientation/orientation-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/sumo/orientation/orientation-test.metta.html): Tests for orientation logic in SUMO.
      - [`orientation.kif.metta`](./extended_compat/hyperon-pln/metta/sumo/orientation/orientation.kif.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/sumo/orientation/orientation.kif.metta.html): Knowledge Interchange Format files for orientation.
    - **[`sumo/located`](././extended_compat/hyperon-pln/metta/sumo/located)**: Pertains to location-based logic.
      - [`located-test.metta`](./extended_compat/hyperon-pln/metta/sumo/located/located-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/sumo/located/located-test.metta.html): Tests for location-based logic in SUMO.
      - [`located.kif.metta`](./extended_compat/hyperon-pln/metta/sumo/located/located.kif.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/sumo/located/located.kif.metta.html): KIF files for location-based logic.
    - **[`sumo/route-between`](././extended_compat/hyperon-pln/metta/sumo/route-between)**: Deals with routing logic.

#### **Synthesis Experiments**

- **[`synthesis/experiments`](./extended_compat/hyperon-pln/metta/synthesis/experiments)**: Experiments in program synthesis.
   - [`synthesize-via-unify-test.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-unify-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-unify-test.metta.html): Tests for synthesis through unification.
   - [`unify-via-case.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/unify-via-case.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/unify-via-case.metta.html): Unification via `case` operator construct.
   - [`synthesize-via-case.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-case.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-case.metta.html): Synthesis through `case` operator construct.
   - [`synthesize-via-let.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-let.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-let.metta.html): Synthesis using the 'let' construct.
   - [`synthesize-via-unify.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-unify.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-unify.metta.html): Synthesis through unification techniques.
   - [`synthesize-via-let-test.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-let-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-let-test.metta.html): Tests for synthesis using the 'let' construct.
   - [`synthesize-via-case-test.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-case-test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-case-test.metta.html): Tests for synthesis through `case` operator construct.
   - [`unify-via-let.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/unify-via-let.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/unify-via-let.metta.html): Unification using the 'let' construct.
   - [`non-determinism.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/non-determinism.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/non-determinism.metta.html): Explorations of non-determinism in synthesis.
   - [`synthesize-via-superpose.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-superpose.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-superpose.metta.html): Synthesis through superposition.
   - [`synthesize-via-type-checking.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-type-checking.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-type-checking.metta.html): Synthesis with type checking.
   - [`self-contained-synthesize.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/self-contained-synthesize.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/self-contained-synthesize.metta.html): Self-contained examples of synthesis.
   - [`synthesize-via-unify-test-longer.metta`](./extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-unify-test-longer.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/synthesis/experiments/synthesize-via-unify-test-longer.metta.html): Extended tests for synthesis through unification.

#### **Match and Unify Operations**

- **[`match`](./extended_compat/hyperon-pln/metta/match)**: Logic for matching and unification.
   - [`DeductionImplicationDirectIntroductionMatchTest.metta`](./extended_compat/hyperon-pln/metta/match/DeductionImplicationDirectIntroductionMatchTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/match/DeductionImplicationDirectIntroductionMatchTest.metta.html): Tests for matching in deduction and direct implication introduction.
   - [`DeductionMatch.metta`](./extended_compat/hyperon-pln/metta/match/DeductionMatch.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/match/DeductionMatch.metta.html): Logic for deduction matching.
   - [`ImplicationDirectIntroductionMatchTest.metta`](./extended_compat/hyperon-pln/metta/match/ImplicationDirectIntroductionMatchTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/match/ImplicationDirectIntroductionMatchTest.metta.html): Tests for direct implication introduction matching.
   - [`DeductionMatchTest.metta`](./extended_compat/hyperon-pln/metta/match/DeductionMatchTest.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/match/DeductionMatchTest.metta.html): Test cases for deduction matching.
   - [`ImplicationDirectIntroductionMatch.metta`](./extended_compat/hyperon-pln/metta/match/ImplicationDirectIntroductionMatch.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/extended_compat/hyperon-pln/metta/match/ImplicationDirectIntroductionMatch.metta.html): Logic for direct implication introduction matching.

#### **Metta-Morph Tests**

- **[`metta-morph_tests`](./baseline_compat/metta-morph_tests)**: Test cases exploring various features and functionalities in MeTTa.
   - [`minnars.metta`](./baseline_compat/metta-morph_tests/minnars.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/minnars.metta.html): Tests and examples for MinNARS implementation.
   - [`sequential_nested.metta`](./baseline_compat/metta-morph_tests/sequential_nested.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/sequential_nested.metta.html): Experiments with sequential and nested structures.
   - [`peano.metta`](./baseline_compat/metta-morph_tests/peano.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/peano.metta.html): Implementations and tests for Peano arithmetic.
   - [`if.metta`](./baseline_compat/metta-morph_tests/if.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/if.metta.html): Tests for 'if' conditional structures.
   - [`types.metta`](./baseline_compat/metta-morph_tests/types.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/types.metta.html): Exploration of type systems and their behaviors.
   - [`states_spaces.metta`](./baseline_compat/metta-morph_tests/states_spaces.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/states_spaces.metta.html): Experiments with state spaces.
   - [`supercollapse.metta`](./baseline_compat/metta-morph_tests/supercollapse.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/supercollapse.metta.html): Tests for collapse operations.
   - [`zeroargs.metta`](./baseline_compat/metta-morph_tests/zeroargs.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/zeroargs.metta.html): Investigating functions with zero arguments.
   - [`match_feval.metta`](./baseline_compat/metta-morph_tests/match_feval.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/match_feval.metta.html): Function evaluation within match structures.
   - [`nalifier.metta`](./baseline_compat/metta-morph_tests/nalifier.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/nalifier.metta.html): NAL (Non-Axiomatic Logic) implementation tests.
   - [`factorial.metta`](./baseline_compat/metta-morph_tests/factorial.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/factorial.metta.html): Factorial function implementations and tests.
   - [`types2.metta`](./baseline_compat/metta-morph_tests/types2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/types2.metta.html): Further exploration into types and their applications.
   - [`and_or.metta`](./baseline_compat/metta-morph_tests/and_or.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/and_or.metta.html): Logical operations and their behavior in different contexts.
   - [`let_superpose_list.metta`](./baseline_compat/metta-morph_tests/let_superpose_list.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/let_superpose_list.metta.html): Experiments with 'let' in list `superpose`.
   - [`multifunction.metta`](./baseline_compat/metta-morph_tests/multifunction.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/multifunction.metta.html): Tests for multifunctional behaviors.
   - [`match_void.metta`](./baseline_compat/metta-morph_tests/match_void.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/match_void.metta.html): Exploring match operations with void returns.
   - [`let_superpose_if_case.metta`](./baseline_compat/metta-morph_tests/let_superpose_if_case.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/let_superpose_if_case.metta.html): Superposing 'let' with 'if' and 'case' structures.
   - [`types3.metta`](./baseline_compat/metta-morph_tests/types3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/types3.metta.html): Advanced type system explorations.
   - [`add_atom_match.metta`](./baseline_compat/metta-morph_tests/add_atom_match.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/add_atom_match.metta.html): Atom addition within match structures.
   - [`let_superpose_list2.metta`](./baseline_compat/metta-morph_tests/let_superpose_list2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/let_superpose_list2.metta.html): Advanced list superposition with 'let'.
   - [`superpose_nested.metta`](./baseline_compat/metta-morph_tests/superpose_nested.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/superpose_nested.metta.html): Nested structure `superpose`.
   - [`identity.metta`](./baseline_compat/metta-morph_tests/identity.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/identity.metta.html): Tests for identity functions and operations.
   - [`nested_parameters.metta`](./baseline_compat/metta-morph_tests/nested_parameters.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/nested_parameters.metta.html): Experiments with nested parameter structures.
   - [`match_superposed_spaces.metta`](./baseline_compat/metta-morph_tests/match_superposed_spaces.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/match_superposed_spaces.metta.html): Matching within superposed spaces.
   - [`collapse.metta`](./baseline_compat/metta-morph_tests/collapse.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/collapse.metta.html): Tests for collapse operations in various contexts.
   - [`letlet.metta`](./baseline_compat/metta-morph_tests/letlet.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/metta-morph_tests/letlet.metta.html): Experiments with nested 'let' structures.

#### **MeTTaLog Sanity Tests**

- **[`hyperon-mettalog_sanity`](./baseline_compat/hyperon-mettalog_sanity)**: Sanity checks and basic tests for Hyperon Mettalog.
   - [`02-curried-plus.metta`](./baseline_compat/hyperon-mettalog_sanity/02-curried-plus.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/02-curried-plus.metta.html): Implementations and tests for curried addition functions.
   - [`00_lang_case.metta`](./baseline_compat/hyperon-mettalog_sanity/00_lang_case.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/00_lang_case.metta.html): Case studies in language design and implementation.
   - [`01_lang_inc.metta`](./baseline_compat/hyperon-mettalog_sanity/01_lang_inc.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/01_lang_inc.metta.html): Increment functions and their usage in language testing.
   - [`04_match_list_like_space.metta`](./baseline_compat/hyperon-mettalog_sanity/04_match_list_like_space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/04_match_list_like_space.metta.html): Matching lists in space-like structures.
   - [`structure-tests.metta`](./baseline_compat/hyperon-mettalog_sanity/structure-tests.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/structure-tests.metta.html): Structural tests for sanity checking.
   - [`05_match_superpose_element_like_space.metta`](./baseline_compat/hyperon-mettalog_sanity/05_match_superpose_element_like_space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/05_match_superpose_element_like_space.metta.html): Matching tests with superposition in element-like spaces.
   - [`00_lang_ok_to_redefine.metta`](./baseline_compat/hyperon-mettalog_sanity/00_lang_ok_to_redefine.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/00_lang_ok_to_redefine.metta.html): Tests for language redefinition capabilities.
   - [`03-soring-via-insert.metta`](./baseline_compat/hyperon-mettalog_sanity/03-soring-via-insert.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/03-soring-via-insert.metta.html): Sorting via insert method.
   - [`06_match_in_space.metta`](./baseline_compat/hyperon-mettalog_sanity/06_match_in_space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/06_match_in_space.metta.html): Matching within space structures.
   - [`base_test_repr_parse_intersection.metta`](./baseline_compat/hyperon-mettalog_sanity/base_test_repr_parse_intersection.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/base_test_repr_parse_intersection.metta.html): Base test for representation and parsing intersection.
   - [`fibo_hang.metta`](./baseline_compat/hyperon-mettalog_sanity/fibo_hang.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/fibo_hang.metta.html): Fibonacci hang test.
   - [`first_answer.metta`](./baseline_compat/hyperon-mettalog_sanity/first_answer.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/first_answer.metta.html): First answer test.
   - [`first_answer_long.metta`](./baseline_compat/hyperon-mettalog_sanity/first_answer_long.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/first_answer_long.metta.html): Extended first answer test.
   - [`intersection_tests.metta`](./baseline_compat/hyperon-mettalog_sanity/intersection_tests.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/intersection_tests.metta.html): Intersection tests.
   - [`is_space.metta`](./baseline_compat/hyperon-mettalog_sanity/is_space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/is_space.metta.html): Space identification test.
   - [`represent_and_parse_tests.metta`](./baseline_compat/hyperon-mettalog_sanity/represent_and_parse_tests.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/represent_and_parse_tests.metta.html): Representation and parsing tests.
   - [`space_intersection_tests.metta`](./baseline_compat/hyperon-mettalog_sanity/space_intersection_tests.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/space_intersection_tests.metta.html): Space intersection tests.
   - [`space_subtraction_tests.metta`](./baseline_compat/hyperon-mettalog_sanity/space_subtraction_tests.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/space_subtraction_tests.metta.html): Space subtraction tests.
   - [`string-tests.metta`](./baseline_compat/hyperon-mettalog_sanity/string-tests.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/string-tests.metta.html): String manipulation tests.
   - [`subtraction_test.metta`](./baseline_compat/hyperon-mettalog_sanity/subtraction_test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/subtraction_test.metta.html): Subtraction operation tests.
   - [`unique_test.metta`](./baseline_compat/hyperon-mettalog_sanity/unique_test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/unique_test.metta.html): Unique value tests.
   - [`xor_test.metta`](./baseline_compat/hyperon-mettalog_sanity/xor_test.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/xor_test.metta.html): XOR operation tests.
   - **[`synth_buffer`](././baseline_compat/hyperon-mettalog_sanity/synth_buffer)**: Synthesis buffer tests.
     - [`synthesize.metta`](./baseline_compat/hyperon-mettalog_sanity/synth_buffer/synthesize.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/synth_buffer/synthesize.metta.html): Synthesis tests.
     - [`time_synthesize.metta`](./baseline_compat/hyperon-mettalog_sanity/synth_buffer/time_synthesize.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/synth_buffer/time_synthesize.metta.html): Time synthesis tests.
     - [`time_synthesize_long.metta`](./baseline_compat/hyperon-mettalog_sanity/synth_buffer/time_synthesize_long.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/synth_buffer/time_synthesize_long.metta.html): Long time synthesis tests.
     - [`time_synthesize_short.metta`](./baseline_compat/hyperon-mettalog_sanity/synth_buffer/time_synthesize_short.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/synth_buffer/time_synthesize_short.metta.html): Short time synthesis tests.

#### **Hyperon Mettalog Sanity Checks**

- **[`hyperon-mettalog_sanity`](./baseline_compat/hyperon-mettalog_sanity)**: Sanity checks and basic tests for Hyperon Mettalog.
   - [`02-curried-plus.metta`](./baseline_compat/hyperon-mettalog_sanity/02-curried-plus.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/02-curried-plus.metta.html): Implementations and tests for curried addition functions.
   - [`00_lang_case.metta`](./baseline_compat/hyperon-mettalog_sanity/00_lang_case.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/00_lang_case.metta.html): Case studies in language design and implementation.
   - [`01_lang_inc.metta`](./baseline_compat/hyperon-mettalog_sanity/01_lang_inc.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/01_lang_inc.metta.html): Increment functions and their usage in language testing.
   - [`04_match_list_like_space.metta`](./baseline_compat/hyperon-mettalog_sanity/04_match_list_like_space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-mettalog_sanity/04_match_list_like_space.metta.html): Matching lists in space-like structures.

#### **Hyperon/MeTTa base Unit Tests**

- **[`hyperon-experimental_scripts`](./baseline_compat/hyperon-experimental_scripts)**: Scripts for experimenting with advanced concepts in Hyperon.
   - [`_e2_states_dia.metta`](./baseline_compat/hyperon-experimental_scripts/_e2_states_dia.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/_e2_states_dia.metta.html): State diagrams and their representation.
   - [`a1_symbols.metta`](./baseline_compat/hyperon-experimental_scripts/a1_symbols.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/a1_symbols.metta.html): Exploration of symbol usage and manipulation.
   - [`d4_type_prop.metta`](./baseline_compat/hyperon-experimental_scripts/d4_type_prop.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/d4_type_prop.metta.html): Type properties and their applications in logic.
   - [`c2_spaces_kb.metta`](./baseline_compat/hyperon-experimental_scripts/c2_spaces_kb.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/c2_spaces_kb.metta.html): Knowledge base management for spaces.
   - [`b1_equal_chain.metta`](./baseline_compat/hyperon-experimental_scripts/b1_equal_chain.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/b1_equal_chain.metta.html): Chain of equalities and logical implications.
   - [`e1_kb_write.metta`](./baseline_compat/hyperon-experimental_scripts/e1_kb_write.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/e1_kb_write.metta.html): Writing and managing knowledge bases.
   - [`d3_deptypes.metta`](./baseline_compat/hyperon-experimental_scripts/d3_deptypes.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/d3_deptypes.metta.html): Explorations of dependent types in logic.
   - [`c3_pln_stv.metta`](./baseline_compat/hyperon-experimental_scripts/c3_pln_stv.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/c3_pln_stv.metta.html): PLN short-term visit logic and reasoning.
   - [`d5_auto_types.metta`](./baseline_compat/hyperon-experimental_scripts/d5_auto_types.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/d5_auto_types.metta.html): Automatic type inference and checking.
   - [`b5_types_prelim.metta`](./baseline_compat/hyperon-experimental_scripts/b5_types_prelim.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/b5_types_prelim.metta.html): Preliminary studies on types in logic.
   - [`e3_match_states.metta`](./baseline_compat/hyperon-experimental_scripts/e3_match_states.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/e3_match_states.metta.html): State matching and logic testing.
   - [`d2_higherfunc.metta`](./baseline_compat/hyperon-experimental_scripts/d2_higherfunc.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/d2_higherfunc.metta.html): Higher-order functions and their applications.
   - [`b3_direct.metta`](./baseline_compat/hyperon-experimental_scripts/b3_direct.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/b3_direct.metta.html): Direct methods in logical reasoning and computation.
   - [`e2_states.metta`](./baseline_compat/hyperon-experimental_scripts/e2_states.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/e2_states.metta.html): State management and logic in complex systems.
   - [`d1_gadt.metta`](./baseline_compat/hyperon-experimental_scripts/d1_gadt.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/d1_gadt.metta.html): Generalized Algebraic Data Types (GADT) exploration.
   - [`c1_grounded_basic.metta`](./baseline_compat/hyperon-experimental_scripts/c1_grounded_basic.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/c1_grounded_basic.metta.html): Grounded basics in logical computation.
   - [`f1_imports.metta`](./baseline_compat/hyperon-experimental_scripts/f1_imports.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/f1_imports.metta.html): Importing external logic and data structures.
	   - [`f1_moduleA.metta`](./baseline_compat/hyperon-experimental_scripts/f1_moduleA.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/f1_moduleA.metta.html): Module A
	   - [`f1_moduleB.metta`](./baseline_compat/hyperon-experimental_scripts/f1_moduleB.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/f1_moduleB.metta.html): Module B
	   - [`f1_moduleC.metta`](./baseline_compat/hyperon-experimental_scripts/f1_moduleC.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/f1_moduleC.metta.html): Module C
   - [`a3_twoside.metta`](./baseline_compat/hyperon-experimental_scripts/a3_twoside.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/a3_twoside.metta.html): Two-sided logic and reasoning experiments.
   - [`b2_backchain.metta`](./baseline_compat/hyperon-experimental_scripts/b2_backchain.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/b2_backchain.metta.html): Backchaining techniques in logical reasoning.
   - [`a2_opencoggy.metta`](./baseline_compat/hyperon-experimental_scripts/a2_opencoggy.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/a2_opencoggy.metta.html): Experiments with OpenCog-like structures.
   - [`b4_nondeterm.metta`](./baseline_compat/hyperon-experimental_scripts/b4_nondeterm.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/b4_nondeterm.metta.html): Non-deterministic approaches in logic.
   - [`c2_spaces.metta`](./baseline_compat/hyperon-experimental_scripts/c2_spaces.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/c2_spaces.metta.html): Space(s) handling and reasoning in logic.
   - [`b0_chaining_prelim.metta`](./baseline_compat/hyperon-experimental_scripts/b0_chaining_prelim.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/baseline_compat/hyperon-experimental_scripts/b0_chaining_prelim.metta.html): Preliminary chaining methods in logic.


### **Feature Demonstrations**

#### **Bidirectional Computation**
- **[`features/bidirectional_computation`](./features/bidirectional_computation)**: Demonstrates bidirectional computation, showcasing how operations can be reversed.
  - [`reverse-arithmetic.metta`](./features/bidirectional_computation/reverse-arithmetic.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/bidirectional_computation/reverse-arithmetic.metta.html): Implements reverse arithmetic operations.
  - [`reverse-functions.metta`](./features/bidirectional_computation/reverse-functions.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/bidirectional_computation/reverse-functions.metta.html): Demonstrates functions operating in reverse.
  - [`send-more.metta`](./features/bidirectional_computation/send-more.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/bidirectional_computation/send-more.metta.html): Solves the SEND+MORE cryptarithmetic puzzle.
  - [`send-more-money.metta`](./features/bidirectional_computation/send-more-money.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/bidirectional_computation/send-more-money.metta.html): Tackles the SEND+MORE=MONEY problem with bidirectional computation.

#### **Containers as Spaces**
- **[`features/containers_are_spaces`](./features/containers_are_spaces)**: Illustrates how containers can act as spatial structures in computation.
  - [`04_match_list_like_space.metta`](./features/containers_are_spaces/04_match_list_like_space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/containers_are_spaces/04_match_list_like_space.metta.html): Demonstrates list-like space matching.
  - [`05_match_superpose_element_like_space.metta`](./features/containers_are_spaces/05_match_superpose_element_like_space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/containers_are_spaces/05_match_superpose_element_like_space.metta.html): Shows superposition in element-like spaces.

#### **Host Language Features**
- **[`features/host_language`](./features/host_language)**: Features leveraging the host programming language(s) capabilities.
  - **[`compiler`](././features/host_language/compiler)**: Compiler-specific features and demonstrations.
    - [`00a_lang_compiled_case.metta`](./features/host_language/compiler/00a_lang_compiled_case.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/host_language/compiler/00a_lang_compiled_case.metta.html): Explores compiled language case studies.
    - [`define_if_like.metta`](./features/host_language/compiler/define_if_like.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/host_language/compiler/define_if_like.metta.html): Introduces compiler features for defining 'if'-like structures.
  - [`httpclient.metta`](./features/host_language/httpclient.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/host_language/httpclient.metta.html): Implements a basic HTTP client.

#### **Loop Checks**
- **[`features/loop-checks`](./features/loop-checks)**: Experiments and tests to check for loops in computations.
  - [`06_loop_0.metta`](./features/loop-checks/06_loop_0.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/loop-checks/06_loop_0.metta.html): Basic loop checking.
  - [`06_loop_1.metta`](./features/loop-checks/06_loop_1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/loop-checks/06_loop_1.metta.html): Intermediate loop checking.
  - [`06_loop_2.metta`](./features/loop-checks/06_loop_2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/loop-checks/06_loop_2.metta.html): Advanced loop checking.

#### **Parallelism**
- **[`features/parallelism`](./features/parallelism)**: Showcases parallel computing with various algorithms and examples.
  - [`builtins.metta`](./features/parallelism/builtins.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/parallelism/builtins.metta.html): Built-in parallel computing functions.


#### **Performance Testing and Benchmarking**

- **[`performance`](./performance)**: Performance tests and benchmarks showcasing optimization and efficiency.
  - [`factorial.metta`](./performance/basic/factorial.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/factorial.metta.html): Factorial function implementation and performance testing.
  - [`factorial.pl`](././performance/basic/factorial.pl)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/factorial.pl.html): Hand-coded optimized version of the factorial function for speed.
  - [`fibo-as-pred.metta`](./performance/basic/fibo-as-pred.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/fibo-as-pred.metta.html): Fibonacci sequence implementation as predicates.
  - [`fibo.metta`](./performance/basic/fibo.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/fibo.metta.html): Basic Fibonacci sequence implementation.
  - [`fibonacci.pl`](././performance/basic/fibonacci.pl)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/fibonacci.pl.html): Hand-coded Fibonacci sequence for optimized performance.
  - [`fwgc1.metta`](./performance/basic/fwgc1.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/fwgc1.metta.html): First version of the Farmer-Wolf-Goat-Cabbage problem.
  - [`fwgc2.metta`](./performance/basic/fwgc2.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/fwgc2.metta.html): Second version of the problem with enhancements.
  - [`fwgc3.metta`](./performance/basic/fwgc3.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/fwgc3.metta.html): Third version, further optimized for performance.
  - [`fwgc.metta`](./performance/basic/fwgc.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/fwgc.metta.html): General implementation of the Farmer-Wolf-Goat-Cabbage problem.
  - [`hanoi-one-space.metta`](./performance/basic/hanoi-one-space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/hanoi-one-space.metta.html): Tower of Hanoi problem in a single space context.
  - [`hanoi-peg-space.metta`](./performance/basic/hanoi-peg-space.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/hanoi-peg-space.metta.html): Tower of Hanoi with multiple pegs and spaces.
  - [`key-lookups-many.metta`](./performance/basic/key-lookups-many.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/key-lookups-many.metta.html): Performance test for key lookups in large datasets.
  - [`pathfinding-easy-f.metta`](./performance/basic/pathfinding-easy-f.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/pathfinding-easy-f.metta.html): Function-based solutions for easy-level pathfinding problems.
  - [`pathfinding-easy.metta`](./performance/basic/pathfinding-easy.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/pathfinding-easy.metta.html): Easy-level pathfinding problems and solutions.
  - [`pathfinding-edge.metta`](./performance/basic/pathfinding-edge.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/pathfinding-edge.metta.html): Edge-based pathfinding algorithms and tests.
  - [`pathfinding-hard-f.metta`](./performance/basic/pathfinding-hard-f.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/pathfinding-hard-f.metta.html): Function-based solutions for hard-level pathfinding problems.
  - [`pathfinding-hard.metta`](./performance/basic/pathfinding-hard.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/pathfinding-hard.metta.html): Hard-level pathfinding problems with complex algorithms.
  - [`pathfinding-med-f.metta`](./performance/basic/pathfinding-med-f.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/pathfinding-med-f.metta.html): Medium difficulty pathfinding problems and function-based solutions.
  - [`state_types.metta`](./performance/basic/state_types.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/state_types.metta.html): Demonstrations of various state types in logical problems.
  - [`talk80.metta`](./performance/basic/talk80.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/talk80.metta.html): Implementation of the classic Talk-80 algorithm for NL parsing and querying.
  - [`test_infer_function_application_type.metta`](./performance/basic/test_infer_function_application_type.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/test_infer_function_application_type.metta.html): Tests for inferring function application types.
  - [`test_list_concatenation.metta`](./performance/basic/test_list_concatenation.metta)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/performance/basic/test_list_concatenation.metta.html): Tests for list concatenation performance.


#### ** MeTTaLog Higher Order Virtual Machine (HVM)**

      known for its pure functional runtime, lazy evaluation, non-garbage collection, and massive parallelism. HVM's beta-optimality allows it, in certain higher-order computations, to be exponentially faster than alternatives, including Haskell's GHC.

- **[`features/hvm`](./features/hvm)**: Demonstrates the capabilities of the Higher-order Virtual Machine (HVM),

  - **[`bugs`](././features/hvm/bugs)**: Bug demonstrations in HVM computations.
    - [`fib_dups.hvm`](./features/hvm/bugs/fib_dups.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/bugs/fib_dups.hvm.html): Illustrates duplication issues in Fibonacci sequence calculation.
    - [`fib_loop.hvm`](./features/hvm/bugs/fib_loop.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/bugs/fib_loop.hvm.html): Demonstrates a looping problem in Fibonacci computation.
    - [`fib_tups.hvm`](./features/hvm/bugs/fib_tups.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/bugs/fib_tups.hvm.html): Showcases tuple-related problems in Fibonacci sequence.
    - [`lotto.hvm`](./features/hvm/bugs/lotto.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/bugs/lotto.hvm.html): Presents a bug example in lottery number generation.

  - **[`callcc`](././features/hvm/callcc)**: Continuation-passing style (CPS) examples in HVM.
    - [`main.hvm`](./features/hvm/callcc/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/callcc/main.hvm.html): Main script demonstrating CPS in HVM.

  - **[`hello`](././features/hvm/hello)**: Basic 'Hello World' example in HVM.
    - [`main.hvm`](./features/hvm/hello/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/hello/main.hvm.html): Simple script to print 'Hello World' in HVM.

  - **[`IO`](././features/hvm/IO)**: Input/Output operations in HVM.
    - [`log.hvm`](./features/hvm/IO/log.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/IO/log.hvm.html): Logging script in HVM.
    - [`query_and_print.hvm`](./features/hvm/IO/query_and_print.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/IO/query_and_print.hvm.html): Script to query and print in HVM.
    - [`store_and_load.hvm`](./features/hvm/IO/store_and_load.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/IO/store_and_load.hvm.html): Demonstrates storing and loading data.

  - **[`lambda`](././features/hvm/lambda)**: Lambda calculus examples in HVM.
    - **[`multiplication`](././features/hvm/lambda/multiplication)**: Multiplication using lambda calculus in HVM.
      - [`better.hvm`](./features/hvm/lambda/multiplication/better.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/lambda/multiplication/better.hvm.html): Improved multiplication implementation.
      - [`main.hvm`](./features/hvm/lambda/multiplication/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/lambda/multiplication/main.hvm.html): Basic multiplication script in lambda calculus.
      - [`main.hs`](./features/hvm/lambda/multiplication/main.hs)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/lambda/multiplication/main.hs.html): Haskell version of the multiplication example.
    - **[`padic_clifford`](././features/hvm/lambda/padic_clifford)**: p-adic numbers and Clifford algebras in HVM.
      - [`main.hvm`](./features/hvm/lambda/padic_clifford/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/lambda/padic_clifford/main.hvm.html): Demonstrates p-adic Clifford algebras.
    - **[`varbase`](././features/hvm/lambda/varbase)**: Variable-based calculations in HVM.
      - [`main.hvm`](./features/hvm/lambda/varbase/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/lambda/varbase/main.hvm.html): Script for variable-based computation in lambda calculus.

  - **[`queue`](././features/hvm/queue)**: Queue implementation in HVM.
    - [`main.hvm`](./features/hvm/queue/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/queue/main.hvm.html): Demonstrates a queue data structure in HVM.

  - **[`sort`](././features/hvm/sort)**: Sorting algorithms implemented in HVM.
    - **[`bitonic`](././features/hvm/sort/bitonic)**: Bitonic sort algorithm in HVM.
      - [`main.hvm`](./features/hvm/sort/bitonic/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/sort/bitonic/main.hvm.html): Main script for bitonic sort.
      - [`main.hs`](./features/hvm/sort/bitonic/main.hs)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/sort/bitonic/main.hs.html): Haskell version of bitonic sort.
    - **[`bubble`](././features/hvm/sort/bubble)**: Bubble sort algorithm in HVM.
      - [`main.hvm`](./features/hvm/sort/bubble/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/sort/bubble/main.hvm.html): Main script for bubble sort.
      - [`main.hs`](./features/hvm/sort/bubble/main.hs)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/sort/bubble/main.hs.html): Haskell version of bubble sort.
    - **[`quick`](././features/hvm/sort/quick)**: Quick sort algorithm in HVM.
      - [`main.hvm`](./features/hvm/sort/quick/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/sort/quick/main.hvm.html): Main script for quick sort.
      - [`main.hs`](./features/hvm/sort/quick/main.hs)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/sort/quick/main.hs.html): Haskell version of quick sort.
    - **[`radix`](././features/hvm/sort/radix)**: Radix sort algorithm in HVM.
      - [`main.hvm`](./features/hvm/sort/radix/main.hvm)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/sort/radix/main.hvm.html): Main script for radix sort.
      - [`main.hs`](./features/hvm/sort/radix/main.hs)[[output]](https://htmlpreview.github.io/?https://raw.githubusercontent.com/logicmoo/hyperon-wam/main/reports/features/hvm/sort/radix/main.hs.html): Haskell version of radix sort.


USER: The above markdown should include also this file tree with generated descriptions

|-- autoexec.metta
|-- baseline_compat
|   |-- anti-regression
|   |   |-- bc_comp.metta
|   |   |-- bchain.metta
|   |   |-- bchain_comp.metta
|   |   |-- comma_is_not_special.metta
|   |   |-- debug_mettalogTestCurried.metta
|   |   |-- mettalogTestCurried.metta
|   |   |-- mettalogTestCurried2.metta
|   |   |-- succAxiom.metta
|   |   |-- system_proofTestCurried.metta
|   |   |-- test_if_closed.metta
|   |   |-- tmpMatch1.metta
|   |   `-- tmpMatch2.metta
|   |-- hyperon-experimental_scripts
|   |   |-- _e2_states_dia.metta
|   |   |-- a1_symbols.metta
|   |   |-- a2_opencoggy.metta
|   |   |-- a3_twoside.metta
|   |   |-- b0_chaining_prelim.metta
|   |   |-- b1_equal_chain.metta
|   |   |-- b2_backchain.metta
|   |   |-- b3_direct.metta
|   |   |-- b4_nondeterm.metta
|   |   |-- b5_types_prelim.metta
|   |   |-- c1_grounded_basic.metta
|   |   |-- c2_spaces.metta
|   |   |-- c2_spaces_kb.metta
|   |   |-- c3_pln_stv.metta
|   |   |-- d1_gadt.metta
|   |   |-- d2_higherfunc.metta
|   |   |-- d3_deptypes.metta
|   |   |-- d4_type_prop.metta
|   |   |-- d5_auto_types.metta
|   |   |-- e1_kb_write.metta
|   |   |-- e2_states.metta
|   |   |-- e3_match_states.metta
|   |   |-- f1_imports.metta
|   |   |-- f1_moduleA.metta
|   |   |-- f1_moduleB.metta
|   |   `-- f1_moduleC.metta
|   |-- hyperon-mettalog_sanity
|   |   |-- 00_lang_case.metta
|   |   |-- 00_lang_ok_to_redefine.metta
|   |   |-- 01_lang_inc.metta
|   |   |-- 02-curried-plus.metta
|   |   |-- 03-soring-via-insert.metta
|   |   |-- 06_match_in_space.metta
|   |   |-- base_test_repr_parse_intersection.metta
|   |   |-- fibo_hang.metta
|   |   |-- first_answer.metta
|   |   |-- first_answer_long.metta
|   |   |-- intersection_tests.metta
|   |   |-- is_space.metta
|   |   |-- represent_and_parse_tests.metta
|   |   |-- space_intersection_tests.metta
|   |   |-- space_subtraction_tests.metta
|   |   |-- string-tests.metta
|   |   |-- structure-tests.metta
|   |   |-- subtraction_test.metta
|   |   |-- synth_buffer
|   |   |   |-- synthesize.metta
|   |   |   |-- synthesize.py
|   |   |   |-- time_synthesize.metta
|   |   |   |-- time_synthesize_long.metta
|   |   |   `-- time_synthesize_short.metta
|   |   |-- unique_test.metta
|   |   `-- xor_test.metta
|   |-- metta-morph_tests
|   |   |-- NARS.metta
|   |   |-- add_atom_match.metta
|   |   |-- and_or.metta
|   |   |-- collapse.metta
|   |   |-- factorial.metta
|   |   |-- hyperpose.metta
|   |   |-- identity.metta
|   |   |-- if.metta
|   |   |-- let_superpose_if_case.metta
|   |   |-- let_superpose_list.metta
|   |   |-- let_superpose_list2.metta
|   |   |-- letlet.metta
|   |   |-- match_feval.metta
|   |   |-- match_superposed_spaces.metta
|   |   |-- match_void.metta
|   |   |-- minnars.metta
|   |   |-- multifunction.metta
|   |   |-- nalifier.metta
|   |   |-- nested_parameters.metta
|   |   |-- peano.metta
|   |   |-- sequential_nested.metta
|   |   |-- states_spaces.metta
|   |   |-- supercollapse.metta
|   |   |-- superpose_nested.metta
|   |   |-- tests0.metta
|   |   |-- types.metta
|   |   |-- types2.metta
|   |   |-- types3.metta
|   |   `-- zeroargs.metta
|   `-- module-system
|       |-- c2_spaces.metta
|       |-- c2_spaces_kb.metta
|       |-- f1_imports.metta
|       |-- f1_moduleA.metta
|       |-- f1_moduleB.metta
|       `-- f1_moduleC.metta



USER: The above markdown should include also this file tree with generated descriptions:






|-- direct_comp
|   |-- 00a_lang_compiled_case.metta
|   |-- add_atom_match.metta
|   |-- compiler_walkthru.metta
|   |-- define_if_like.metta
|   |-- transpiler
|   |   |-- identity.metta
|   |   |-- let_superpose_if_case.metta
|   |   |-- let_superpose_list.metta
|   |   |-- let_superpose_list2.metta
|   |   |-- letlet.metta
|   |   |-- match_feval.metta
|   |   `-- peano.metta
|   `-- walk_thru
|       |-- bc_comp.metta
|       |-- bc_impl.metta
|       |-- bchain.metta
|       |-- bchain_comp.metta
|       |-- comma_is_not_special.metta
|       |-- debug_mettalogTestCurried.metta
|       |-- impl_mettalogTestCurried.metta
|       |-- mettalogTestCurried.metta
|       |-- mettalogTestCurried2.metta
|       |-- succAxiom.metta
|       |-- system_proofTestCurried.metta
|       |-- test_if_closed.metta
|       |-- tmpMatch1.metta
|       `-- tmpMatch2.metta
|-- extended_compat
|   |-- ai-service
|   |   |-- ai-service-composition-easy.metta
|   |   |-- ai-service-composition-hard.metta
|   |   `-- ai-service-composition-sanity.metta
|   |-- bio-atomspace
|   |   |-- cellxgene
|   |   |   `-- edges.metta
|   |   |-- gencode
|   |   |   `-- edges.metta
|   |   |-- gtex
|   |   |   `-- eqtl
|   |   |       `-- edges.metta
|   |   |-- onotology
|   |   |   |-- edges.metta
|   |   |   `-- nodes.metta
|   |   |-- pln
|   |   |   |-- curried-chainer.metta
|   |   |   `-- gene-pln-xp.metta
|   |   |-- pln-xp-mettalog.metta
|   |   |-- pln-xp.metta
|   |   |-- query
|   |   |   `-- gene-query.metta
|   |   |-- reactome
|   |   |   `-- nodes.metta
|   |   |-- rules.metta
|   |   |-- sample_kb_v2.metta
|   |   |-- tadmap
|   |   |   `-- edges.metta
|   |   |-- type_defs.metta
|   |   `-- uniprot
|   |       |-- edges.metta
|   |       `-- nodes.metta
|   |-- check_types
|   |   |-- check_types-temp
|   |   |   |-- b5_types_prelim.metta
|   |   |   |-- c1_grounded_basic.metta
|   |   |   |-- soring-via-insert.metta
|   |   |   `-- type_prop.metta
|   |   |-- d1_gadt.metta
|   |   |-- d2_higherfunc.metta
|   |   |-- d3_deptypes.metta
|   |   |-- d4_type_prop.metta
|   |   |-- d5_auto_types.metta
|   |   `-- e2_states.metta
|   |-- hyperon-miner
|   |   |-- data
|   |   |   `-- sample.metta
|   |   |-- dependent-types
|   |   |   |-- MinerCurriedDTL.metta
|   |   |   |-- MinerCurriedDTLTest.metta
|   |   |   |-- MinerDTL.metta
|   |   |   |-- MinerDTL1.metta
|   |   |   |-- MinerDTL1Test.metta
|   |   |   |-- MinerDTL2.metta
|   |   |   |-- MinerDTL2Test.metta
|   |   |   |-- MinerDTL3.metta
|   |   |   |-- MinerDTL3Test.metta
|   |   |   |-- MinerDTLTest.metta
|   |   |   |-- SupportRuleExp.metta
|   |   |   |-- bchain.metta
|   |   |   |-- chainer.metta
|   |   |   `-- succAxiom.metta
|   |   |-- match
|   |   |   |-- MinerMatch.metta
|   |   |   |-- MinerMatchTest.metta
|   |   |   |-- result.metta
|   |   |   `-- tmpMatch.metta
|   |   |-- result.metta
|   |   `-- utils
|   |       |-- DeBruijnIndex.metta
|   |       |-- MinerUtils.metta
|   |       |-- MinerUtils~OLD.metta
|   |       |-- helper.py
|   |       `-- utils-test.metta
|   |-- hyperon-pln
|   |   `-- metta
|   |       |-- backward-chainer
|   |       |   `-- backward-chainer-xp.metta
|   |       |-- backward-chaining
|   |       |   `-- bc-xp.metta
|   |       |-- common
|   |       |   |-- BelieveMe.metta
|   |       |   |-- EqualityType.metta
|   |       |   |-- EqualityTypeTest.metta
|   |       |   |-- In.metta
|   |       |   |-- InTest.metta
|   |       |   |-- List.metta
|   |       |   |-- ListTest.metta
|   |       |   |-- Maybe.metta
|   |       |   |-- MaybeTest.metta
|   |       |   |-- Num.metta
|   |       |   |-- Num8.metta
|   |       |   |-- NumTest.metta
|   |       |   |-- OrderedSet.metta
|   |       |   |-- OrderedSetTest.metta
|   |       |   |-- Record.metta
|   |       |   |-- formula
|   |       |   |   |-- DeductionFormula.metta
|   |       |   |   |-- DeductionFormulaTest.metta
|   |       |   |   |-- ImplicationDirectIntroductionFormula.metta
|   |       |   |   `-- ModusPonensFormula.metta
|   |       |   `-- truthvalue
|   |       |       |-- EvidentialTruthValue.metta
|   |       |       |-- EvidentialTruthValueTest.metta
|   |       |       |-- MeasEq.metta
|   |       |       |-- TemporalTruthValue.metta
|   |       |       |-- TruthValue.metta
|   |       |       `-- TruthValueTest.metta
|   |       |-- curried-chaining
|   |       |   `-- curried-chainer.metta
|   |       |-- dependent-types
|   |       |   |-- DeductionDTL.metta
|   |       |   |-- DeductionDTLTest.metta
|   |       |   |-- DeductionImplicationDirectIntroductionDTLTest.metta
|   |       |   |-- ImplicationDirectIntroductionDTL.metta
|   |       |   |-- ImplicationDirectIntroductionDTLTest.metta
|   |       |   |-- ModusPonensDTL.metta
|   |       |   `-- ModusPonensDTLTest.metta
|   |       |-- entail
|   |       |   |-- DeductionEntail.metta
|   |       |   |-- DeductionEntailTest.metta
|   |       |   |-- ImplicationDirectIntroductionEntail.metta
|   |       |   `-- ImplicationDirectIntroductionEntailTest.metta
|   |       |-- equal
|   |       |   |-- DeductionEqual.metta
|   |       |   |-- DeductionEqualTest.metta
|   |       |   |-- ImplicationDirectIntroductionEqual.metta
|   |       |   `-- ImplicationDirectIntroductionEqualTest.metta
|   |       |-- forward-chainer
|   |       |   |-- forward-chainer-test.metta
|   |       |   `-- forward-chainer-xp.metta
|   |       |-- forward-chaining
|   |       |   `-- fc-xp.metta
|   |       |-- hol
|   |       |   |-- ListTest.metta
|   |       |   |-- NatDTLTest.metta
|   |       |   |-- NatParityTest.metta
|   |       |   |-- NatSimpleTest.metta
|   |       |   |-- NatSimpleTest~OLD.metta
|   |       |   |-- NatStandaloneTest.metta
|   |       |   |-- NatTest.metta
|   |       |   `-- NatTest~OLD.metta
|   |       |-- inference-control
|   |       |   |-- inf-ctl-month-bc-cont-xp.metta
|   |       |   |-- inf-ctl-month-bc-xp.metta
|   |       |   |-- inf-ctl-month-xp.metta
|   |       |   `-- inf-ctl-xp.metta
|   |       |-- iterative-chaining
|   |       |   |-- ibc-xp.metta
|   |       |   `-- ifc-xp.metta
|   |       |-- match
|   |       |   |-- DeductionImplicationDirectIntroductionMatchTest.metta
|   |       |   |-- DeductionMatch.metta
|   |       |   |-- DeductionMatchTest.metta
|   |       |   |-- ImplicationDirectIntroductionMatch.metta
|   |       |   `-- ImplicationDirectIntroductionMatchTest.metta
|   |       |-- pln
|   |       |   |-- dependent-types
|   |       |   |   |-- DeductionDTL.metta
|   |       |   |   |-- DeductionDTLTest.metta
|   |       |   |   |-- DeductionImplicationDirectIntroductionDTLTest.metta
|   |       |   |   |-- ImplicationDirectIntroductionDTL.metta
|   |       |   |   |-- ImplicationDirectIntroductionDTLTest.metta
|   |       |   |   |-- ModusPonensDTL.metta
|   |       |   |   `-- ModusPonensDTLTest.metta
|   |       |   |-- entail
|   |       |   |   |-- DeductionEntail.metta
|   |       |   |   |-- DeductionEntailTest.metta
|   |       |   |   |-- ImplicationDirectIntroductionEntail.metta
|   |       |   |   `-- ImplicationDirectIntroductionEntailTest.metta
|   |       |   |-- equal
|   |       |   |   |-- DeductionEqual.metta
|   |       |   |   |-- DeductionEqualTest.metta
|   |       |   |   |-- ImplicationDirectIntroductionEqual.metta
|   |       |   |   `-- ImplicationDirectIntroductionEqualTest.metta
|   |       |   `-- match
|   |       |       |-- DeductionImplicationDirectIntroductionMatchTest.metta
|   |       |       |-- DeductionMatch.metta
|   |       |       |-- DeductionMatchTest.metta
|   |       |       |-- ImplicationDirectIntroductionMatch.metta
|   |       |       `-- ImplicationDirectIntroductionMatchTest.metta
|   |       |-- polyward-chaining
|   |       |   `-- pc-xp.metta
|   |       |-- subtyping
|   |       |   |-- rule-base.metta
|   |       |   `-- subtyping-test.metta
|   |       |-- sumo
|   |       |   |-- john-carry-flower
|   |       |   |   |-- john-carry-flower-test.metta
|   |       |   |   `-- john-carry-flower.kif.metta
|   |       |   |-- load-suo-kif.metta
|   |       |   |-- located
|   |       |   |   |-- located-test.metta
|   |       |   |   `-- located.kif.metta
|   |       |   |-- orientation
|   |       |   |   |-- orientation-test.metta
|   |       |   |   `-- orientation.kif.metta
|   |       |   `-- rule-base.metta
|   |       `-- synthesis
|   |           |-- Synthesize.metta
|   |           |-- SynthesizeTest.metta
|   |           |-- Unify.metta
|   |           |-- UnifyTest.metta
|   |           `-- experiments
|   |               |-- non-determinism.metta
|   |               |-- self-contained-synthesize.metta
|   |               |-- synthesize-via-case-test.metta
|   |               |-- synthesize-via-case.metta
|   |               |-- synthesize-via-let-test.metta
|   |               |-- synthesize-via-let.metta
|   |               |-- synthesize-via-superpose.metta
|   |               |-- synthesize-via-type-checking.metta
|   |               |-- synthesize-via-unify-test-longer.metta
|   |               |-- synthesize-via-unify-test.metta
|   |               |-- synthesize-via-unify.metta
|   |               |-- unify-via-case.metta
|   |               `-- unify-via-let.metta


As well as these:

|   |-- metta-examples
|   |   |-- SICP_book
|   |   |   |-- additional_funcs.py
|   |   |   |-- chapter_1_1.metta
|   |   |   |-- chapter_1_2.metta
|   |   |   |-- chapter_1_3.metta
|   |   |   |-- chapter_1_3_2.metta
|   |   |   |-- chapter_2_1.metta
|   |   |   |-- chapter_2_2.metta
|   |   |   |-- chapter_2_3.metta
|   |   |   |-- chapter_2_4.metta
|   |   |   `-- draw_line.py
|   |   |-- aunt-kg
|   |   |   |-- adameve.metta
|   |   |   |-- adameve_simple.metta
|   |   |   |-- baseline_formulation.metta
|   |   |   |-- introduction.metta
|   |   |   |-- json_to_metta.py
|   |   |   |-- lordOfTheRings.metta
|   |   |   |-- lordOfTheRings_simple.metta
|   |   |   |-- royal92.metta
|   |   |   |-- royal92_simple.metta
|   |   |   |-- sergey_rodionov_formulation.metta
|   |   |   |-- simple_conversion.metta
|   |   |   |-- simpsons.metta
|   |   |   |-- simpsons_simple.metta
|   |   |   `-- toy.metta
|   |   |-- child_ai
|   |   |   `-- child_ai.py
|   |   |-- collatz
|   |   |   |-- classical.metta
|   |   |   `-- peano.metta
|   |   |-- combinator-calculi
|   |   |   |-- tree.metta
|   |   |   `-- y_comb_examples.metta
|   |   |-- combinator_logic_experiments
|   |   |   `-- y_comb_examples.metta
|   |   |-- edges-to-edges
|   |   |   |-- InteractiveDisneyStrategy.metta
|   |   |   |-- json_to_metta.py
|   |   |   `-- nte.metta
|   |   |-- fizzbuzz
|   |   |   |-- classical.metta
|   |   |   |-- constructive.metta
|   |   |   `-- simple.metta
|   |   |-- game_of_clue
|   |   |   `-- game_of_clue.metta
|   |   |-- logic
|   |   |   |-- all_any.metta
|   |   |   |-- memb.metta
|   |   |   |-- puzzle.metta
|   |   |   `-- puzzle.py
|   |   |-- metta_amr
|   |   |   |-- amr_matching
|   |   |   |   |-- __init__.py
|   |   |   |   |-- amr_matcher.py
|   |   |   |   `-- amr_template_instance.py
|   |   |   |-- amr_processing
|   |   |   |   |-- __init__.py
|   |   |   |   |-- amr_processor.py
|   |   |   |   |-- triple_processor.py
|   |   |   |   `-- type_detector.py
|   |   |   |-- experiments
|   |   |   |   |-- amr_template_nlu.py
|   |   |   |   |-- loader_test.py
|   |   |   |   |-- matcher_tests.py
|   |   |   |   |-- performance_tests.py
|   |   |   |   |-- test_spec_amrmatcher.py
|   |   |   |   `-- unit_tests.py
|   |   |   |-- metta_space
|   |   |   |   |-- __init__.py
|   |   |   |   |-- metta_space.py
|   |   |   |   `-- pattern_parser.py
|   |   |   `-- metta_to_amr
|   |   |       |-- amr-connect.metta
|   |   |       |-- dep-types.metta
|   |   |       |-- instances.metta
|   |   |       |-- user.metta
|   |   |       `-- utterance.py
|   |   |-- perfect-numbers
|   |   |   `-- basic.metta
|   |   |-- prob-dep-types
|   |   |   |-- inf_order_probs.metta
|   |   |   `-- prob_dep_types.metta
|   |   |-- python_from_metta
|   |   |   |-- default.metta
|   |   |   |-- fs.metta
|   |   |   `-- requests.metta
|   |   |-- recursion-schemes
|   |   |   |-- base.py
|   |   |   |-- run.py
|   |   |   `-- src
|   |   |       |-- base.metta
|   |   |       |-- examples
|   |   |       |   |-- benchmark.metta
|   |   |       |   `-- expression.metta
|   |   |       `-- schemes.metta
|   |   |-- red-black-lambda
|   |   |   |-- lambda-theory.metta
|   |   |   `-- red-black.metta
|   |   |-- stack-based
|   |   |   |-- interpreter.metta
|   |   |   `-- rewrites.metta
|   |   |-- strips
|   |   |   |-- strips-to-metta-flat
|   |   |   |   |-- blocks-domain_flat.metta
|   |   |   |   |-- logistics-i-1_flat.metta
|   |   |   |   |-- queries_flat.metta
|   |   |   |   `-- strips_to_metta_flat.py
|   |   |   `-- strips-to-metta-improved
|   |   |       |-- blocks-i-0.metta
|   |   |       |-- blocks-i-1.metta
|   |   |       |-- logistics-i-1.metta
|   |   |       |-- queries.metta
|   |   |       `-- strips_to_metta.py
|   |   `-- traverser
|   |       |-- basic.metta
|   |       |-- creation.metta
|   |       |-- json_to_metta.py
|   |       `-- tinkerpop-modern.metta

As well as these:

|-- features
|   |-- bidirectional_computation
|   |   |-- fish-riddle.metta
|   |   |-- relative-arithmetic.metta
|   |   |-- reverse-arithmetic.metta
|   |   |-- reverse-functions.metta
|   |   |-- send-more-money.metta
|   |   `-- send-more.metta
|   |-- containers_are_spaces
|   |   |-- 04_match_list_like_space.metta
|   |   `-- 05_match_superpose_element_like_space.metta
|   |-- debugging
|   |   |-- debug_none.metta
|   |   |-- debug_some_more.metta
|   |   |-- hyperon_experimental_issue_481.metta
|   |   |-- hyperon_experimental_issue_492.metta
|   |   |-- hyperon_experimental_issue_500.metta
|   |   |-- hyperon_experimental_issue_514.metta
|   |   |-- hyperon_experimental_issue_516.metta
|   |   |-- hyperon_experimental_issue_516_redduced.metta
|   |   `-- hyperon_experimental_issue_530.metta
|   |-- distributed-processing
|   |   |-- create-server.metta
|   |   |-- server-with-memberchk.metta
|   |   |-- server1-with-some-data.metta
|   |   |-- server2-with-some-data.metta
|   |   |-- use-one-server.metta
|   |   `-- use-two-servers.metta
|   |-- exception_handling
|   |   |-- catch_throw_1.metta
|   |   |-- catch_throw_2.metta
|   |   `-- catch_throw_3.metta
|   |-- host_language
|   |   |-- fallback_functions.metta
|   |   `-- httpclient.metta
|   |-- intersection_tests.metta
|   |-- io
|   |   `-- file-output.metta
|   |-- iterators
|   |   |-- combined.metta
|   |   `-- lazy_test.metta
|   |-- loop-checks
|   |   |-- 06_loop_0.metta
|   |   |-- 06_loop_1.metta
|   |   `-- 06_loop_2.metta
|   |-- multidirectional
|   |   |-- append.metta
|   |   |-- append_using_equality.metta
|   |   |-- ioioioi.metta
|   |   |-- ioioioi_using_equality.metta
|   |   |-- mfm_ioio.metta
|   |   |-- obif.metta
|   |   |-- obif_clp.metta
|   |   `-- obif_easy.metta
|   |-- parallelism
|   |   `-- builtins.metta
|   |-- planner
|   |   |-- monkey-banana-v1.metta
|   |   |-- monkey-banana-v2.metta
|   |   `-- project-task-managment.metta
|   |-- polymorphic-types
|   |   |-- examples-objects.metta
|   |   `-- examples-quants.metta
|   |-- possibly_wrong
|   |   `-- too_eager_math.metta
|   |-- represent_and_parse_tests.metta
|   |-- structures
|   |   `-- composing.metta
|   `-- typed-variables
|       |-- enforced-types.metta
|       |-- even-number.metta
|       `-- variable-pattern.metta
|-- flybase
|   |-- extra
|   |   |-- nodes_med.metta
|   |   |-- nodes_sm.metta
|   |   |-- pmquery.metta
|   |   `-- proofexample.metta
|   |-- flybase-deduced-connections.metta
|   |-- flybase-deduced-queries.metta
|   |-- flybase-deduced-types.metta
|   |-- flybase-loader-size-estimates.metta
|   |-- flybase-loader.metta
|   |-- flybase-vspace.metta
|   |-- from_das
|   |   |-- flybase_rust_uses_python_das.metta
|   |   |-- mettalog_das_client.py
|   |   |-- mettalog_inserts_to_das.metta
|   |   |-- mettalog_uses_das.metta
|   |   `-- script_example_BIO.py
|   |-- from_rust
|   |   `-- flybase_rust_only.metta
|   |-- from_vspace
|   |   `-- flybase_rust_uses_python_vspace.metta
|   |-- output~
|   |   |-- flybase-deduced.metta
|   |   |-- flybase-mined-flat.metta
|   |   `-- flybase-mined.metta
|   `-- sanity
|       |-- download_file.metta
|       |-- load_all_of_flybase.metta
|       |-- simple_query1.metta
|       `-- simple_query2_llm.metta
|-- more-anti-regression
|   |-- chaining
|   |   |-- backward_chain.metta
|   |   |-- bc-xp.metta
|   |   |-- bc_comp_1.metta
|   |   |-- bc_comp_2.metta
|   |   |-- chaining_prelim.metta
|   |   |-- go_rel.metta
|   |   |-- parent_go.metta
|   |   |-- parent_go_1.metta
|   |   |-- parent_go_1_comp.metta
|   |   |-- parent_go_comp.metta
|   |   |-- pln-xp-local_v2.metta
|   |   |-- pln-xp.metta
|   |   |-- pln_stv.metta
|   |   |-- rules.metta
|   |   |-- rules_v2.metta
|   |   |-- sample_kb.metta
|   |   |-- sample_kb_v2.metta
|   |   |-- sample_kb_v2_coexpressed_both.metta
|   |   |-- sample_kb_v2_coexpressed_once.metta
|   |   |-- sample_kb_v2_tiny.metta
|   |   |-- tadmap_edges.metta
|   |   |-- type_prop.metta
|   |   |-- types_prelim.metta
|   |   |-- util.py
|   |   |-- v2-pln-xp-easy-impl.metta
|   |   |-- v2-pln-xp-sys-impl.metta
|   |   `-- v2-pln-xp.metta
|   |-- constraint
|   |   `-- types.metta
|   |-- introspect
|   |   |-- exam-spaces.metta
|   |   |-- show-space-self.metta
|   |   |-- show-space.metta
|   |   `-- show-type.metta
|   |-- minimal-metta
|   |   |-- stdlib_minimal.metta
|   |   `-- stdlib_minimal_test.metta
|   |-- spaces
|   |   |-- add-remove-match-float.metta
|   |   |-- add-remove-match-integer.metta
|   |   |-- add-remove-match-mix-float-integer.metta
|   |   |-- add-remove-match-s-strings.metta
|   |   |-- add-remove-match-s-symbols.metta
|   |   |-- add-remove-match-ss-strings.metta
|   |   |-- add-remove-match-ss-symbols.metta
|   |   |-- add-remove-match-strings.metta
|   |   `-- add-remove-match-symbols.metta
|   |-- std
|   |   |-- animals.metta
|   |   |-- example_pm_queries.metta
|   |   |-- gadt.metta
|   |   |-- get-atoms-test.metta
|   |   |-- grounded_basic.metta
|   |   |-- higher_order_funcs.metta
|   |   |-- kb_write.metta
|   |   |-- lte.metta
|   |   |-- nondeterm.metta
|   |   |-- spaces_kb.metta
|   |   `-- symbols.metta
|   `-- stdlib-mettalog
|       |-- interpreter_minimal.rs.metta
|       |-- stdlib.rs.metta
|       |-- stdlib_mettalog_test.metta
|       `-- stdlib_mettalog_test_pt2.metta
|-- nars_interp
|   `-- nars
|       `-- main-branch
|           |-- NARS.metta
|           |-- NARS_Prev.metta
|           |-- minnars_Prev.metta
|           |-- nalifier_Prev.metta
|           |-- prereqs.metta
|           |-- tests0.metta
|           |-- tests0_Prev.metta
|           |-- tests1.metta
|           |-- tests1_pre.metta
|           |-- tests2.metta
|           |-- tests3.metta
|           |-- tests4.metta
|           `-- tests5.metta
|-- nars_w_comp
|   `-- nars
|       `-- main-branch
|           |-- LIB_NARS.metta
|           |-- LIB_NARS_COMP_1.metta
|           |-- NARS.metta
|           |-- NARS_OPT.metta
|           |-- TEST_METTA_tests0.metta
|           |-- TEST_METTA_tests1.metta
|           |-- TEST_METTA_tests2.metta
|           |-- TEST_METTA_tests3.metta
|           |-- TEST_METTA_tests4.metta
|           |-- TEST_METTA_tests5.metta
|           |-- tests0.metta
|           |-- tests1.metta
|           |-- tests2.metta
|           |-- tests3.metta
|           |-- tests4.metta
|           `-- tests5.metta
|-- performance
|   |-- basic
|   |   |-- cml-unshared.metta
|   |   |-- coins.metta
|   |   |-- factorial.metta
|   |   |-- fibo-as-pred.metta
|   |   |-- fibo.metta
|   |   |-- fibonacci.metta
|   |   |-- fwgc.metta
|   |   |-- fwgc1.metta
|   |   |-- fwgc2.metta
|   |   |-- fwgc3.metta
|   |   |-- hanoi-one-space.metta
|   |   |-- hanoi-peg-space.metta
|   |   |-- hyperposing-shared.metta
|   |   |-- hyperposing-unshared.metta
|   |   |-- hyperposing.metta
|   |   |-- key-lookups-many.metta
|   |   |-- pathfinding-easy-f.metta
|   |   |-- pathfinding-easy.metta
|   |   |-- pathfinding-edge.metta
|   |   |-- pathfinding-hard-f.metta
|   |   |-- pathfinding-hard.metta
|   |   |-- pathfinding-med-f.metta
|   |   |-- state_types.metta
|   |   |-- talk80.metta
|   |   |-- test_infer_function_application_type.metta
|   |   `-- test_list_concatenation.metta
|   |-- comparisons
|   |   |-- add-atom-naive-fib.metta
|   |   |-- fibo_10i.metta
|   |   |-- fibo_20i.metta
|   |   |-- fibo_444444c.metta
|   |   |-- fibo_80c.metta
|   |   |-- fibo_900c.metta
|   |   |-- fibo_Arg1i.metta
|   |   |-- fibo_arg1c.metta
|   |   |-- fibo_hang_900.metta
|   |   |-- inf-ctl-month-xp-long.metta
|   |   |-- inf-ctl-month-xp-short.metta
|   |   |-- inf-ctl-month-xp-short_1.metta
|   |   |-- inf-ctl-month-xp-short_3.metta
|   |   |-- inf-ctl-month-xp-short_5.metta
|   |   |-- inf-ctl-month-xp-short_6.metta
|   |   |-- naive-fib.metta
|   |   |-- nils_if_control_test_10.metta
|   |   |-- nils_if_control_test_2.metta
|   |   |-- nils_if_control_test_3.metta
|   |   |-- nils_if_control_test_4.metta
|   |   |-- nils_if_control_test_5.metta
|   |   `-- nils_if_control_test_6.metta
|   |-- gpt2-like
|   |   |-- corpus
|   |   |   |-- bigram-sentence-evaluator
|   |   |   |   |-- bigram_model.metta
|   |   |   |   |-- bigrams.metta
|   |   |   |   |-- calc_similarity.metta
|   |   |   |   |-- hmm_tagger.metta
|   |   |   |   |-- sigmas.metta
|   |   |   |   |-- tags_count.metta
|   |   |   |   |-- taus.metta
|   |   |   |   |-- trigramModel.metta
|   |   |   |   |-- unigrams.metta
|   |   |   |   `-- wordVectors.metta
|   |   |   |-- self_dialogue_corpus
|   |   |   |   |-- old
|   |   |   |   |   `-- get_data.py
|   |   |   |   `-- train_from_topic_harry_potter.txt.metta
|   |   |   |-- soap_opera_corpus
|   |   |   |   `-- so_convert.metta
|   |   |   `-- tmpdata
|   |   |       |-- checkpoint_is_word_1.metta
|   |   |       |-- checkpoint_is_word_2.metta
|   |   |       |-- checkpoint_ngram_5.metta
|   |   |       |-- checkpoint_ngram_6.metta
|   |   |       |-- checkpoint_tok_split_3.metta
|   |   |       |-- checkpoint_tok_split_4.metta
|   |   |       |-- checkpoint_training_3.metta
|   |   |       |-- checkpoint_trigram_3.metta
|   |   |       |-- checkpoint_trigram_4.metta
|   |   |       |-- done_$toplevel_converting_3.metta
|   |   |       |-- done_pllm_is_word_1.metta
|   |   |       |-- done_pllm_is_word_2.metta
|   |   |       |-- done_pllm_ngram_5.metta
|   |   |       |-- done_pllm_ngram_6.metta
|   |   |       |-- done_pllm_ngram_7.metta
|   |   |       |-- done_pllm_ngram_8.metta
|   |   |       |-- done_pllm_ngram_9.metta
|   |   |       |-- done_pllm_tok_split_3.metta
|   |   |       |-- done_pllm_tok_split_4.metta
|   |   |       |-- done_pllm_training_3.metta
|   |   |       |-- done_pllm_training_4.metta
|   |   |       |-- done_pllm_trigram_3.metta
|   |   |       |-- done_pllm_trigram_4.metta
|   |   |       |-- done_so_convert_converting_3.metta
|   |   |       |-- done_so_convert_is_word_1.metta
|   |   |       |-- done_so_convert_is_word_2.metta
|   |   |       |-- done_so_convert_ngram_5.metta
|   |   |       |-- done_so_convert_ngram_6.metta
|   |   |       |-- done_so_convert_tok_split_3.metta
|   |   |       |-- done_so_convert_tok_split_4.metta
|   |   |       |-- done_so_convert_trigram_3.metta
|   |   |       |-- done_so_convert_trigram_4.metta
|   |   |       |-- done_tmp_buffer_training_2.metta
|   |   |       |-- done_training_3.metta
|   |   |       |-- done_user_is_word_1.metta
|   |   |       |-- done_user_is_word_2.metta
|   |   |       |-- done_user_ngram_5.metta
|   |   |       |-- done_user_ngram_6.metta
|   |   |       |-- done_user_ngram_7.metta
|   |   |       |-- done_user_ngram_8.metta
|   |   |       |-- done_user_ngram_9.metta
|   |   |       |-- done_user_tok_split_4.metta
|   |   |       |-- done_user_training_3.metta
|   |   |       |-- done_user_training_4.metta
|   |   |       |-- done_user_trigram_4.metta
|   |   |       `-- more_pllm_training_3.metta
|   |   |-- language_models
|   |   |   |-- pllm_drs.metta
|   |   |   |-- pllm_lps.metta
|   |   |   |-- pllm_penntree.metta
|   |   |   |-- plm.metta
|   |   |   |-- training.metta
|   |   |   |-- training_bi.metta
|   |   |   |-- training_logicmoo.metta
|   |   |   |-- training_terms.metta
|   |   |   |-- trains_trigrams.metta
|   |   |   |-- unweighted_pllm.metta
|   |   |   |-- utils_pllm.metta
|   |   |   `-- weighted_pllm.metta
|   |   `-- logicmoo_pllm.metta

As well as these:

|   |-- knowledge_graphs
|   |   `-- graphml
|   |       |-- graphml_csv_test.metta
|   |       `-- graphml_test.metta
|   `-- nondet_unify
|       |-- boyer.metta
|       |-- browse.metta
|       |-- chat_parser.metta
|       |-- crypt.metta
|       |-- derive.metta
|       |-- divide10.metta
|       |-- fast_mu.metta
|       |-- flatten.metta
|       |-- log10.metta
|       |-- meta_qsort.metta
|       |-- mu.metta
|       |-- nand.metta
|       |-- nreverse.metta
|       |-- ops8.metta
|       |-- perfect.metta
|       |-- poly_10.metta
|       |-- prover.metta
|       |-- qsort.metta
|       |-- queens_8.metta
|       |-- query.metta
|       |-- reducer.metta
|       |-- run.metta
|       |-- sendmore.metta
|       |-- serialise.metta
|       |-- simple_analyzer.metta
|       |-- tak.metta
|       |-- times10.metta
|       |-- unify.metta
|       `-- zebra.metta
|-- python_compat
|   |-- extend
|   |   |-- TEMP.metta
|   |   |-- compileme.metta
|   |   |-- example1.metta
|   |   |-- example2.metta
|   |   |-- example3.metta
|   |   |-- example4.metta
|   |   |-- example5.metta
|   |   |-- example6.metta
|   |   |-- mettamorph.py
|   |   `-- test.py
|   |-- hyperon-experimental_python
|   |   |-- integration
|   |   |   |-- test_das.metta
|   |   |   `-- test_torch.py
|   |   |-- sandbox
|   |   |   |-- bhv_binding
|   |   |   |   |-- 01_example_majority.metta
|   |   |   |   |-- 02_example_perm.metta
|   |   |   |   |-- 03_example_dict.metta
|   |   |   |   |-- 04_example_dollar_of_mexico.metta
|   |   |   |   `-- bhv_binding.py
|   |   |   |-- das_gate
|   |   |   |   |-- dasgate.py
|   |   |   |   `-- test_das.metta
|   |   |   |-- neurospace
|   |   |   |   |-- llm_gate.py
|   |   |   |   |-- neurospace.py
|   |   |   |   |-- test_assist.metta
|   |   |   |   `-- test_nspace.metta
|   |   |   |-- numpy
|   |   |   |   |-- nm_test.metta
|   |   |   |   `-- numme.py
|   |   |   |-- pytorch
|   |   |   |   |-- kwargsme.py
|   |   |   |   |-- parse_torch_func_signatures.py
|   |   |   |   |-- parsing_exceptions.py
|   |   |   |   |-- tm_test.metta
|   |   |   |   |-- tm_test.py
|   |   |   |   `-- torchme.py
|   |   |   |-- repl
|   |   |   |   `-- metta_repl.py
|   |   |   |-- resolve
|   |   |   |   |-- r.metta
|   |   |   |   |-- r.py
|   |   |   |   `-- resolve.py
|   |   |   `-- sql_space
|   |   |       |-- sql_space.py
|   |   |       `-- sql_space_test.metta
|   |   `-- tests
|   |       |-- error_pyext.py
|   |       |-- ext_dir
|   |       |   |-- __init__.py
|   |       |   `-- ext.py
|   |       |-- extension.py
|   |       |-- pyfile_test_mod.py
|   |       |-- test_atom.py
|   |       |-- test_atom_type.py
|   |       |-- test_bindings.py
|   |       |-- test_common.py
|   |       |-- test_custom_space.py
|   |       |-- test_environment.py
|   |       |-- test_examples.py
|   |       |-- test_extend.py
|   |       |-- test_grounded_type.py
|   |       |-- test_grounding_space.py
|   |       |-- test_load.metta
|   |       |-- test_load.py
|   |       |-- test_metta.py
|   |       |-- test_minecraft.py
|   |       |-- test_minelogy.py
|   |       |-- test_modules.py
|   |       |-- test_pln_tv.py
|   |       |-- test_run_metta.py
|   |       |-- test_sexparser.py
|   |       `-- test_stdlib.py
|   |-- janus
|   |   |-- janus_api.metta
|   |   |-- metta_calls_python_in_janus.metta
|   |   |-- mymodule.py
|   |   `-- python_calls_metta_in_janus.metta
|   |-- metta-motto
|   |   |-- examples
|   |   |   |-- __init__.py
|   |   |   |-- answer_cache
|   |   |   |   `-- example1.py
|   |   |   |-- bio_ai
|   |   |   |   |-- bio-ai.metta
|   |   |   |   |-- data_subset
|   |   |   |   |   |-- gaf
|   |   |   |   |   |   `-- edges.metta
|   |   |   |   |   |-- gencode
|   |   |   |   |   |   |-- edges.metta
|   |   |   |   |   |   `-- nodes.metta
|   |   |   |   |   |-- gtex
|   |   |   |   |   |   `-- eqtl
|   |   |   |   |   |       `-- edges.metta
|   |   |   |   |   |-- ontology
|   |   |   |   |   |   |-- edges.metta
|   |   |   |   |   |   `-- nodes.metta
|   |   |   |   |   |-- reactome
|   |   |   |   |   |   |-- edges.metta
|   |   |   |   |   |   `-- nodes.metta
|   |   |   |   |   `-- uniprot
|   |   |   |   |       |-- edges.metta
|   |   |   |   |       `-- nodes.metta
|   |   |   |   |-- lobo_test_bio_agent.py
|   |   |   |   |-- mettalog-bio-ai.metta
|   |   |   |   |-- mlog_test_bio_agent.py
|   |   |   |   |-- override-bio-ai.metta
|   |   |   |   `-- test_bio_agent.py
|   |   |   |-- langchain_examples
|   |   |   |   |-- test_langchain_agent.metta
|   |   |   |   `-- test_langchain_tools_agent.metta
|   |   |   |-- test_anthropic.metta
|   |   |   |-- test_assist.metta
|   |   |   |-- test_guide.metta
|   |   |   |-- test_guide_func.metta
|   |   |   |-- test_guide_with_history.metta
|   |   |   |-- test_llms.py
|   |   |   |-- test_open_router_agent.metta
|   |   |   |-- test_retrieval.metta
|   |   |   |-- test_sparql.metta
|   |   |   `-- test_sparql_wikidata.metta
|   |   |-- motto
|   |   |   |-- __init__.py
|   |   |   |-- agents
|   |   |   |   |-- __init__.py
|   |   |   |   |-- agent.py
|   |   |   |   |-- anthropic_agent.py
|   |   |   |   |-- data_processors
|   |   |   |   |   |-- __init__.py
|   |   |   |   |   |-- doc_processor.py
|   |   |   |   |   `-- embedings_getters.py
|   |   |   |   |-- gpt_agent.py
|   |   |   |   |-- messages_processor.py
|   |   |   |   |-- metta_agent.py
|   |   |   |   |-- openrouter_agent.py
|   |   |   |   `-- retrieval_agent.py
|   |   |   |-- langchain_agents
|   |   |   |   |-- __init__.py
|   |   |   |   |-- langchain_agent.py
|   |   |   |   `-- langchain_states.metta
|   |   |   |-- llm_gate.py
|   |   |   |-- sparql_gate
|   |   |   |   |-- __init__.py
|   |   |   |   `-- sparql_gate.py
|   |   |   `-- utils.py
|   |   |-- setup.py
|   |   |-- tests
|   |   |   |-- basic_agent_call.metta
|   |   |   |-- basic_agent_script.metta
|   |   |   |-- basic_agent_stateful.metta
|   |   |   |-- basic_direct_call.metta
|   |   |   |-- basic_function_call.metta
|   |   |   |-- basic_script_call.metta
|   |   |   |-- metta_chat.metta
|   |   |   |-- nested_dialog_call.metta
|   |   |   |-- nested_script_direct.metta
|   |   |   |-- sparql_functions_test.metta
|   |   |   |-- test_custom_agent.py
|   |   |   |-- test_python_direct.py
|   |   |   |-- test_scripts.py
|   |   |   `-- test_sparql_select.py
|   |   `-- tutorial
|   |       |-- 00_basic_chatpgt.py
|   |       |-- 01_basic_chatgpt.metta
|   |       |-- 02_metta_agent.py
|   |       |-- 03_agent_call.metta
|   |       |-- 04_prompt_call.metta
|   |       |-- 05_call_prompt.py
|   |       |-- 06_logic.py
|   |       |-- 06b_reason.metta
|   |       |-- 07_dialog.metta
|   |       |-- 07_dialog.py
|   |       |-- 08_nested_dialog.py
|   |       |-- 09_agent_stateful.py
|   |       |-- 09_generate_query_dbpedia.metta
|   |       |-- 10_generate_query_wikidata.metta
|   |       |-- app1_generate_query_dbpedia.metta
|   |       `-- app2_generate_query_wikidata.metta
|   |-- mettamorph.metta
|   `-- timing
|       |-- timing.metta
|       `-- timing.py
`-- settings.metta


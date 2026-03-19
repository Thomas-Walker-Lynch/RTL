# Harmony

Harmony provides a language-agnostic project directory structure and maintenance tools for long-lived software development. It enforces clarity about where things live, utilizing role-based work areas and the strict separation of skeleton, team member authored, machine-made, and third-party software.

While designed to manage the complexity of multi-person teams, Harmony serves equally well for an individual developer. A single person can easily assume the roles of administrator, developer, and tester simultaneously by opening separate emacs buffers or terminal windows for each role.

## Bootstrapping a New Project

**Important:** Do not begin writing code directly in a fresh clone of this skeleton. You must decouple your new project from the Harmony version control history to prevent accidentally committing your project files back to the upstream skeleton.

To create a Harmony-based project, the project administrator performs these steps:

1. Clone the Harmony project to a local directory.
2. Remove the `.git` tree.
3. Rename the Harmony directory to the name of the new project.
4. Rename the `0pus_Harmony` file to reflect the name of the new project.
5. Add a line to the `shared/tool/version` file for the new project.
6. Run `git init -b core_developer_branch` to start your own repository.

## Viewing the Documentation

To view the project documentation with its intended formatting, a person must provide the RT style library.  One way to do this is to clone the styling repository side-by-side with your new project directory, and then to link it into the third-party directory.

From the parent directory of your new project, clone the required style repository:

```bash
cd ..
git clone -b release_v1 https://github.com/Thomas-Walker-Lynch/RT-style-JS_public
```

Then, from the root of your new project repository, link it:

```bash
cd shared/third_party
ln -s ../../../RT-style-JS_public RT-style-JS_public
```

Find an introductory document at `document/Introduction_to_Harmony.html'. After the style library is installed, clicking on it in file navigator should open it in a browser.

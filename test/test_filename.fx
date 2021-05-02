/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

from UTest import *
import Filename

TEST("filename.regression", fun() {
    val cwd = "/home/joe/project"
    val sep = Filename.dir_sep()

    val reg_data = [:
        ("note.txt", ".", "note.txt", "note", cwd+sep+"note.txt"),
        (".config", ".", ".config", ".config", cwd+sep+".config"),
        ("README", ".", "README", "README", cwd+sep+"README"),
        ("/", sep, sep, "/", "/"),
        (".", ".", ".", ".", cwd+sep+"."),
        ("/usr", "/", "usr", "/usr", "/usr"),
        ("../foo/bar.1/test", "../foo/bar.1", "test",
            "../foo/bar.1/test", f"/home/joe{sep}foo/bar.1/test"),
        ("../../shared/Книги/Kinder-\ und\ Hausmärchen.pdf",
        "../../shared/Книги", "Kinder- und Hausmärchen.pdf",
        "../../shared/Книги/Kinder- und Hausmärchen",
        f"/home{sep}shared/Книги/Kinder- und Hausmärchen.pdf"),
        ("./doc/tutorial.md", "./doc", "tutorial.md",
            "./doc/tutorial", cwd+sep+"doc/tutorial.md"),
    :]

    for (path, dir, basename, no_ext, norm) <- reg_data {
        EXPECT_EQ(Filename.dirname(path), dir)
        EXPECT_EQ(Filename.basename(path), basename)
        EXPECT_EQ(Filename.remove_extension(path), no_ext)
        EXPECT_EQ(Filename.normalize(cwd, path), norm)
    }
})

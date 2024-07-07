local ls = require("luasnip")
-- some shorthands...
local s = ls.snippet
local sn = ls.snippet_node
local text_node = ls.text_node
local insert_node = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local l = require("luasnip.extras").lambda
local rep = require("luasnip.extras").rep
local p = require("luasnip.extras").partial
local m = require("luasnip.extras").match
local n = require("luasnip.extras").nonempty
local dl = require("luasnip.extras").dynamic_lambda
local format = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local types = require("luasnip.util.types")
local conds = require("luasnip.extras.expand_conditions")


ls.add_snippets("perl", {
    ls.snippet(
        "sub",
        format(
            [[

            =head1 {1}

            {2}

            =head2 Args:

            =over

            =item {3} ({4}): {5}

            =back

            =head2 Returns:

            {6}: {7}

            =cut

            sub {8}
            {{
                my ${9} = shift @_;
            }}
            ]],
            {
                insert_node(1, "sub-name"),
                insert_node(2, "sub-desc"),
                insert_node(3, "first-arg-name"),
                insert_node(4, "first-arg-type"),
                insert_node(5, "first-arg-desc"),
                insert_node(6, "return-type"),
                insert_node(7, "return-desc"),
                rep(1),
                rep(3)
            }
        )
    ),

    ls.snippet(
        "if",
        format(
            [[
            if ({1})
            {{
                {2}
            }}
            ]],
            {
                insert_node(1, "if-condition"),
                insert_node(2, "if-contents"),
            }
        )
    ),

    ls.snippet(
        "ife",
        format(
            [[
            if ({1})
            {{
                {2}
            }}
            else
            {{
                {3}
            }}
            ]],
            {
                insert_node(1, "if-condition"),
                insert_node(2, "if-contents"),
                insert_node(3, "else-contents"),
            }
        )
    ),

    ls.snippet(
        "ifee",
        format(
            [[
            if ({1})
            {{
                {2}
            }}
            elsif ({3})
            {{
                {4}
            }}
            else
            {{
                {5}
            }}
            ]],
            {
                insert_node(1, "if-condition"),
                insert_node(2, "if-contents"),
                insert_node(3, "elsif-condition"),
                insert_node(4, "elsif-contents"),
                insert_node(5, "else-contents"),
            }
        )
    ),

    ls.snippet(
        "for",
        format(
            [[
            foreach my ${1} ({2})
            {{
                {3}
            }}
            ]],
            {
                insert_node(1, "loop_var"),
                insert_node(2, "loop-elements"),
                insert_node(3, "body"),
            }
        )
    ),

    ls.snippet(
        "open",
        format(
            [[
            open(my ${1}, "{2}", {3}) or die "Could not open {4}: $!";
            close({5});
            ]],
            {
                insert_node(1, "file-handle_name"),
                insert_node(2, "read-write-type"),
                insert_node(3, "file-path"),
                rep(3),
                rep(1),
            }
        )
    ),

    ls.snippet(
        "testsimple",
        format(
            [[
            #! /usr/bin/env perl
            
            # Enfore perl best practices
            use warnings;
            use strict;
            
            
            #-------------------------------------------------------------------------------
            # Dependencies
            #-------------------------------------------------------------------------------
            use Test2::V0;
            use {1};
            
            
            #-------------------------------------------------------------------------------
            # Test execution
            #-------------------------------------------------------------------------------
            is( 1, 1, "simple test");
            
            
            done_testing();

            ]],
            {
                insert_node(1, "package-to-test"),
            }
        )
    ),

    ls.snippet(
        "package",
        format(
            [[
            package {1};

            # Enfore perl best practices
            use strict;
            use warnings;


            #-------------------------------------------------------------------------------
            # Dependencies
            #-------------------------------------------------------------------------------
            

            #-------------------------------------------------------------------------------
            # Exported Functions
            #-------------------------------------------------------------------------------


            #-------------------------------------------------------------------------------
            # Internal Functions
            #-------------------------------------------------------------------------------


            1;


            __END__

            =pod

            =head1 {2}

            {3}

            =head2 SYNOPSIS

              use {4};

              # TODO

            =cut

            ]],
            {
                insert_node(1, "package-name"),
                rep(1),
                insert_node(2, "description"),
                rep(1)
            }
        )
    ),

    ls.snippet(
        "ppod",
        format(
            [[
            =pod

            =head1 {1}

            {2}

            =head2 SYNOPSIS

              use {3};

              # TODO

            =cut

            ]],
            {
                insert_node(1, "package-name"),
                insert_node(2, "description"),
                rep(1),
            }
        )
    ),
}, { key = "perl" })

ls.add_snippets("perl", {
    ls.snippet(
        "#!",
        {
            text_node({"#! /usr/bin/env perl", "", "use strict;", "use warnings;", "", ""}),
        }
    ),
}, { key = "perl-auto", type = "autosnippets" })


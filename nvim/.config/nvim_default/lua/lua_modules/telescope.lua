local telescope = require('telescope')

telescope.setup {}

-- To get fzf loaded and working with telescope,
-- you need to call load_extension, somewhere after
-- the setup function.
telescope.load_extension('fzf')


vim.keymap.set("n", "<leader>ff", require('telescope.builtin').find_files)
vim.keymap.set("n", "<leader>fg", require('telescope.builtin').live_grep)
vim.keymap.set("n", "<leader>fb", require('telescope.builtin').buffers)
vim.keymap.set("n", "<leader>fb", require('telescope.builtin').help_tags)


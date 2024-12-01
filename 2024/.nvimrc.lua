require'lspconfig'.gleam.setup{}
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.gleam", -- Match Gleam files
  callback = function()
    vim.lsp.buf.format({ async = false }) -- Format synchronously before saving
  end,
})

import { defineConfig } from 'vitepress'
import footnote from 'markdown-it-footnote'
import { nav, sidebar } from '../src/navigation.mjs'

export default defineConfig({
  base: '/et/',
  srcDir: 'src',
  title: 'et.el',
  description: 'A type checker for Emacs Lisp',
  lastUpdated: true,
  markdown: {
    config(md) {
      md.use(footnote)
    }
  },
  themeConfig: {
    nav,
    sidebar,
    outline: { level: [2, 4] },
    search: { provider: 'local' }
  }
})

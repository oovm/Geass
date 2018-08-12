module.exports = {
	dest: 'docs/.build',
	locales: {
		'/': {
			lang: 'zh-CN',
			title: 'Geis',
			description: 'Code ➤ GEASS'
		}
	},
	head: [
		['link', {rel: 'stylesheet', href: 'https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.5.1/katex.min.css'}]
	],
	themeConfig: {
		repo: 'Moe-Net/Geass',
		editLinks: true,
		docsDir: 'docs',
		markdown: {
			lineNumbers: true
		},
		sidebar: [
			{
				title: '开发文档',
				children: [
					'/Start/',
					'/Start/Developer.md',
					'/Start/Editor.md',
					'/Start/EditorAdv.md'
				]
			},
			{
				title: 'MagicSquare 模块',
				children: [
					'/MagicSquare/'
				]
			}
		]
	},
	serviceWorker: true,
	markdown: {
		config: md => {
			md.use(require("markdown-it-katex"));
		}
	}
};

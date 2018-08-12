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
		['link', {rel: 'stylesheet', href: 'https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.css'}]
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
				title: 'ExCode 模块',
				children: [
					'/ExCode/'
				]
			},
			{
				title: 'ExForm 模块',
				children: [
					'/ExForm/'
				]
			},
			{
				title: 'ExRandom 模块',
				children: [
					'/ExRandom/'
				]
			},
			{
				title: 'SortVisual 模块',
				children: [
					'/SortVisual/'
				]
			},
			{
				title: 'Isomerism 模块',
				children: [
					'/Isomerism/'
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

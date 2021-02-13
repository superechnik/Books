import { DataGrid } from '@material-ui/data-grid';

const columns = [
    { field: 'id', headerName: 'Id', width: 70 },
    { field: 'title', headerName: 'Title', width: 130 },
    { field: 'author', headerName: 'Author', width: 130},
  ];
  
  const rows = [
    { id: 1, title: 'about all', author: 'Francis Gibbet' },
    { id: 2, title: 'about none', author: 'Parry Semi' },
    
  ];

  export default function BookGrid() {
    return (
      <div style={{ height: 400, width: '100%', color: 'whitesmoke' }}>
        <DataGrid rows={rows} columns={columns} pageSize={5} />
      </div>
    );
  }